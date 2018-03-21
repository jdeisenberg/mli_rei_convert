/*
 * These are the possible states that the parser can be in
 * SimplePre is preformatted text in [ ]
 * MultiLine is preformatted text in {[  [}
 */
type scanState =
  | Scan
  | OpenParen
  | PossibleComment
  | PossibleDocComment
  | InComment
  | InDocComment
  | SimplePre
  | PossibleMultiLine
  | MultiLine
  | PossibleEndMultiLine
  | PossibleEndComment;
  
/*
 * This is a record used to keep track of the progress of parsing
 * the document comments
 */
type statusType = {
  state: scanState,
  prevState: scanState,
  inStr: string,
  position: int, /* easier to keep track of this than constantly update the string being scanned */
  toConvert: string, /* the currently collected ML to convert to Reason */
  finalString: string
};

/*
 * Should we use parseML/printRE or parseMLI/printREI?
 */
type conversionType =
  | Implementation
  | Interface;

/*
 * Convert a scanState to a string; used for debugging
 */
let string_of_state = (state: scanState) : string => {
  switch (state) {
  | Scan => "Scan"
  | OpenParen => "OpenParen"
  | PossibleComment => "PossibleComment"
  | PossibleDocComment => "PossibleDocComment"
  | InComment => "InComment"
  | InDocComment => "InDocComment"
  | SimplePre => "SimplePre"
  | PossibleMultiLine => "PossibleMultiLine"
  | MultiLine => "MultiLine"
  | PossibleEndMultiLine => "PossibleEndMultiLine"
  | PossibleEndComment => "PossibleEndComment"
  }
};
  
/*
 * Utility functions for conversion of ML to RE
 */
let toReason = (s: string, implOrInterface: conversionType) : string => {
  Js.String.replaceByRe([%re "/\\\\@/g"], "@", s) |> switch (implOrInterface) {
        | Implementation => Refmt.parseML
        | Interface => Refmt.parseMLI
      }
    |> fun
        | Ok(ast) => ast |> 
          switch(implOrInterface) {
            | Implementation => Refmt.printRE
            | Interface => Refmt.printREI
          }
        | Error(`RefmtParseError({message})) => "Error: " ++ message
};

/*  The following two functions are needed when processing multi-line examples */
/*
 * Return count of leading newlines, leading spaces (after newlines),
 * and string without leading newlines and spaces.
 */
let countLeadingWhitespace = (s: string) : (int, int, string) => {
  let sansNewlines = Js.String.replaceByRe([%re "/^\\n+/g"], "", s);
  let sansLeading = Js.String.replaceByRe([%re "/^\\s+/"], "", sansNewlines);
  (Js.String.length(s) - Js.String.length(sansNewlines),
    Js.String.length(sansNewlines) - Js.String.length(sansLeading), sansLeading)
};

/*
 * Add the given number of spaces to each line in the given string
 */
let addSpaces = (n: int, str: string) => {
  let spaces = String.make(n, ' ');
  Js.String.split("\n", str) |>
    Array.map((item: string) : string => { spaces ++ item }) |>
    Js.Array.joinWith("\n");
};

/*
 * Convert multi-line OCaml to Reason. If the result begins with Error,
 * return original string. Because refmt adds an extra newline at the
 * end of its conversion and trims leading space, we have to save
 * spacing and re-install it.
 */
let multiLineToRE = (implOrInterface: conversionType, s: string) : string => {
  let (nNewlines, nSpaces, inputString) = countLeadingWhitespace(s);
  let result = (toReason(inputString, implOrInterface) |>
    Js.String.replaceByRe([%re "/\\n\\n/g"], "\n") |>
    Js.String.trim) ++ "\n";
  /* Js.log("multiLineToRE |" ++ inputString ++ "| -> |" ++ result ++ "|"); */
  
  if (Js.String.startsWith("Error:", result)) {
    "!!!" ++ s ++ "!!!"
  } else {
    String.make(nNewlines, '\n') ++ addSpaces(nSpaces, result);
  }
};

/*
 * Convert single line of OCaml to Reason. If the result begins with Error,
 * return original string. Get rid of any trailing \n and semicolons.
 */
let singleLineToRE = (implOrInterface: conversionType, s: string) : string => {
  let result = toReason(s, implOrInterface) |>
    Js.String.trim |>
    Js.String.replaceByRe([%re "/;$/"], "");
  /* Js.log("singleLineToRE |" ++ s ++ "| -> |" ++ result ++ "|"); */
  if (Js.String.startsWith("Error:", result)) {
    "!!!" ++ s ++ "!!!"
  } else {
    result;
  }
};

/*
 * Utility functions to keep status updated without having to repeat
 * all the fields. (Have you noticed that I like utility functions?)
 */
let emit = (ch: string, newState: scanState, status: statusType) : statusType => {
  {state: newState, prevState: status.state, position: status.position + 1,
    toConvert: status.toConvert, finalString: status.finalString ++ ch, inStr: status.inStr}
};

let collect = (ch: string, newState: scanState, status: statusType) : statusType => {
  {state: newState, prevState: status.state, position: status.position + 1,
    toConvert: status.toConvert ++ ch, finalString: status.finalString, inStr: status.inStr}
};

/*
 * Here is where all the work is done, going through the input string
 * one character at a time and keeping track of state
 */
let rec convert = (status: statusType) : string => {
  if (status.position == Js.String.length(status.inStr)) {
    status.finalString;
  } else {
    let ch = Js.String.get(status.inStr, status.position);
    /* Js.log("Current character: " ++ ch ++ " state: " ++ string_of_state(status.state)); */
    switch (status.state, ch) {
      | (Scan, "(") => convert(emit("(",  OpenParen, status))
      | (Scan, _) => convert(emit(ch, Scan, status))
      | (OpenParen, "*") => convert(emit("*", PossibleDocComment, status))
      | (OpenParen, _) => convert(emit(ch, Scan, status))
      | (PossibleDocComment, "*") => convert(emit(ch, InDocComment, status))
      | (PossibleDocComment, _) => convert(emit(ch, InComment, status))
      | (PossibleComment, _) => convert(emit(ch, InComment, status))
      | (InComment, "*") => convert(emit(ch, PossibleEndComment, status))
      | (InComment, _) => convert(emit(ch, InComment, status))
      | (PossibleEndComment, ")") => convert(emit(ch, Scan, status))
      | (PossibleEndComment, _) => convert(emit(ch, status.prevState, status))
      | (InDocComment, "*") => convert(emit(ch, PossibleEndComment, status))
      | (InDocComment, "[") => convert(emit(ch, SimplePre, {...status, toConvert: ""}))
      | (InDocComment, "{") => convert(emit(ch, PossibleMultiLine, status))
      | (InDocComment, _) => convert(emit(ch, InDocComment, status))
      | (PossibleMultiLine, "[") => convert(emit(ch, MultiLine, status))
      | (PossibleMultiLine, _) => convert(emit(ch, InDocComment, status))
      | (SimplePre, "]") => convert({
          state: InDocComment,
          prevState: status.state,
          inStr: status.inStr,
          toConvert: "",
          finalString: status.finalString ++ singleLineToRE(Implementation, status.toConvert) ++ "]",
          position: status.position + 1})
      | (SimplePre, _) => convert(collect(ch, SimplePre, status))
      | (MultiLine, "]") => convert(collect("", PossibleEndMultiLine, status))
      | (MultiLine, _) => convert(collect(ch, MultiLine, status))
      | (PossibleEndMultiLine, "}") => convert({
          state: InDocComment,
          prevState: status.state,
          inStr: status.inStr,
          toConvert: "",
          finalString: status.finalString ++ multiLineToRE(Implementation, status.toConvert) ++ "]}",
          position: status.position + 1})
      | (PossibleEndMultiLine, _) => {
          convert(collect("]" ++ ch, MultiLine, status))
        }
    }
  }
};

/*
 * Given an input file name, construct an output file name by replacing
 * .mli with .rei, and then:
 * 1) Open the input file
 * 2) Convert all the doc comments with the convert() function
 * 3) Let to_reason() convert the entire file, which takes care of non-comment code
 * 4) Write to the output file
 */
let processFile = (fileName: string) : unit => {
  let outName = Js.String.replaceByRe([%re "/mli$/g"], "", fileName) ++ "rei";
  let inStr = Node.Fs.readFileAsUtf8Sync(fileName);
  convert({state: Scan, prevState: Scan,
    position: 0, toConvert: "", finalString: "", inStr: inStr}) |> 
    toReason(_, Interface) |>
    Node.Fs.writeFileAsUtf8Sync(outName);
};

/* Skip argument 0 (the program name) */
Arg.current := 1;
Arg.parse([], processFile, "doc_convert file ...");
