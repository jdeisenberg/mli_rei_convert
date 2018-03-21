// Generated by BUCKLESCRIPT VERSION 2.2.2, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Arg = require("bs-platform/lib/js/arg.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Refmt = require("bs-refmt/src/Refmt.bs.js");
var $$String = require("bs-platform/lib/js/string.js");

function string_of_state(state) {
  switch (state) {
    case 0 : 
        return "Scan";
    case 1 : 
        return "OpenParen";
    case 2 : 
        return "PossibleComment";
    case 3 : 
        return "PossibleDocComment";
    case 4 : 
        return "InComment";
    case 5 : 
        return "InDocComment";
    case 6 : 
        return "SimplePre";
    case 7 : 
        return "PossibleMultiLine";
    case 8 : 
        return "MultiLine";
    case 9 : 
        return "PossibleEndMultiLine";
    case 10 : 
        return "PossibleEndComment";
    
  }
}

function toReason(s, implOrInterface) {
  var param = Curry._1(implOrInterface !== 0 ? Refmt.parseMLI : Refmt.parseML, s);
  if (param.tag) {
    return "Error: " + param[0][1][/* message */0];
  } else {
    return Curry._1(implOrInterface !== 0 ? Refmt.printREI : Refmt.printRE, param[0]);
  }
}

function countLeadingWhitespace(s) {
  var sansNewlines = s.replace((/^\n+/g), "");
  var sansLeading = sansNewlines.replace((/^\s+/), "");
  return /* tuple */[
          s.length - sansNewlines.length | 0,
          sansNewlines.length - sansLeading.length | 0,
          sansLeading
        ];
}

function addSpaces(n, str) {
  var spaces = $$String.make(n, /* " " */32);
  return $$Array.map((function (item) {
                  return spaces + item;
                }), str.split("\n")).join("\n");
}

function multiLineToRE(implOrInterface, s) {
  var match = countLeadingWhitespace(s);
  var result = toReason(match[2], implOrInterface).replace((/\n\n/g), "\n").trim() + "\n";
  if (result.startsWith("Error:")) {
    return "!!!" + (s + "!!!");
  } else {
    return $$String.make(match[0], /* "\n" */10) + addSpaces(match[1], result);
  }
}

function singleLineToRE(implOrInterface, s) {
  var result = toReason(s, implOrInterface).trim().replace((/;$/), "");
  if (result.startsWith("Error:")) {
    return "!!!" + (s + "!!!");
  } else {
    return result;
  }
}

function emit(ch, newState, status) {
  return /* record */[
          /* state */newState,
          /* prevState */status[/* state */0],
          /* inStr */status[/* inStr */2],
          /* position */status[/* position */3] + 1 | 0,
          /* toConvert */status[/* toConvert */4],
          /* finalString */status[/* finalString */5] + ch
        ];
}

function collect(ch, newState, status) {
  return /* record */[
          /* state */newState,
          /* prevState */status[/* state */0],
          /* inStr */status[/* inStr */2],
          /* position */status[/* position */3] + 1 | 0,
          /* toConvert */status[/* toConvert */4] + ch,
          /* finalString */status[/* finalString */5]
        ];
}

function convert(_status) {
  while(true) {
    var status = _status;
    if (status[/* position */3] === status[/* inStr */2].length) {
      return status[/* finalString */5];
    } else {
      var ch = status[/* inStr */2][status[/* position */3]];
      var match = status[/* state */0];
      switch (match) {
        case 0 : 
            if (ch === "(") {
              _status = emit("(", /* OpenParen */1, status);
              continue ;
              
            } else {
              _status = emit(ch, /* Scan */0, status);
              continue ;
              
            }
            break;
        case 1 : 
            if (ch === "*") {
              _status = emit("*", /* PossibleDocComment */3, status);
              continue ;
              
            } else {
              _status = emit(ch, /* Scan */0, status);
              continue ;
              
            }
            break;
        case 2 : 
            _status = emit(ch, /* InComment */4, status);
            continue ;
            case 3 : 
            if (ch === "*") {
              _status = emit(ch, /* InDocComment */5, status);
              continue ;
              
            } else {
              _status = emit(ch, /* InComment */4, status);
              continue ;
              
            }
            break;
        case 4 : 
            if (ch === "*") {
              _status = emit(ch, /* PossibleEndComment */10, status);
              continue ;
              
            } else {
              _status = emit(ch, /* InComment */4, status);
              continue ;
              
            }
            break;
        case 5 : 
            switch (ch) {
              case "*" : 
                  _status = emit(ch, /* PossibleEndComment */10, status);
                  continue ;
                  case "[" : 
                  var newrecord = status.slice();
                  _status = emit(ch, /* SimplePre */6, (newrecord[/* toConvert */4] = "", newrecord));
                  continue ;
                  case "{" : 
                  _status = emit(ch, /* PossibleMultiLine */7, status);
                  continue ;
                  default:
                _status = emit(ch, /* InDocComment */5, status);
                continue ;
                
            }
            break;
        case 6 : 
            if (ch === "]") {
              _status = /* record */[
                /* state : InDocComment */5,
                /* prevState */status[/* state */0],
                /* inStr */status[/* inStr */2],
                /* position */status[/* position */3] + 1 | 0,
                /* toConvert */"",
                /* finalString */status[/* finalString */5] + (singleLineToRE(/* Implementation */0, status[/* toConvert */4]) + "]")
              ];
              continue ;
              
            } else {
              _status = collect(ch, /* SimplePre */6, status);
              continue ;
              
            }
            break;
        case 7 : 
            if (ch === "[") {
              _status = emit(ch, /* MultiLine */8, status);
              continue ;
              
            } else {
              _status = emit(ch, /* InDocComment */5, status);
              continue ;
              
            }
            break;
        case 8 : 
            if (ch === "]") {
              _status = collect("", /* PossibleEndMultiLine */9, status);
              continue ;
              
            } else {
              _status = collect(ch, /* MultiLine */8, status);
              continue ;
              
            }
            break;
        case 9 : 
            if (ch === "}") {
              _status = /* record */[
                /* state : InDocComment */5,
                /* prevState */status[/* state */0],
                /* inStr */status[/* inStr */2],
                /* position */status[/* position */3] + 1 | 0,
                /* toConvert */"",
                /* finalString */status[/* finalString */5] + (multiLineToRE(/* Implementation */0, status[/* toConvert */4]) + "]}")
              ];
              continue ;
              
            } else {
              _status = collect("]" + ch, /* MultiLine */8, status);
              continue ;
              
            }
            break;
        case 10 : 
            if (ch === ")") {
              _status = emit(ch, /* Scan */0, status);
              continue ;
              
            } else {
              _status = emit(ch, status[/* prevState */1], status);
              continue ;
              
            }
            break;
        
      }
    }
  };
}

function processFile(fileName) {
  var outName = fileName.replace((/mli$/g), "") + "rei";
  var inStr = Fs.readFileSync(fileName, "utf8");
  Fs.writeFileSync(outName, toReason(convert(/* record */[
                /* state : Scan */0,
                /* prevState : Scan */0,
                /* inStr */inStr,
                /* position */0,
                /* toConvert */"",
                /* finalString */""
              ]), /* Interface */1), "utf8");
  return /* () */0;
}

Arg.current[0] = 1;

Arg.parse(/* [] */0, processFile, "doc_convert file ...");

exports.string_of_state = string_of_state;
exports.toReason = toReason;
exports.countLeadingWhitespace = countLeadingWhitespace;
exports.addSpaces = addSpaces;
exports.multiLineToRE = multiLineToRE;
exports.singleLineToRE = singleLineToRE;
exports.emit = emit;
exports.collect = collect;
exports.convert = convert;
exports.processFile = processFile;
/*  Not a pure module */
