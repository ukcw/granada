import {
  Box,
  Button,
  Container,
  Flex,
  HStack,
  Textarea,
} from "@chakra-ui/react";
import React, { useState } from "react";
import {
  getError,
  isError,
  logOutput,
  resetErrorSettings,
  resetLogOutput,
  setPrintTrue,
  startingEEnv,
  startingTEnv,
} from "../scilla_parser/general";
import {
  ADTValue,
  BNumLit,
  Bystr,
  BystrX,
  Clo,
  Int128L,
  Int256L,
  Int32L,
  Int64L,
  Map,
  Msg,
  StringLit,
  TAbs,
  Uint128L,
  Uint256L,
  Uint32L,
  Uint64L,
} from "../scilla_parser/literals";
import scillaLexer from "../scilla_parser/scillaLexer";
import scillaParser from "../scilla_parser/scillaParser";
import SyntaxVisitor from "../scilla_parser/syntaxVisitor";
import _ from "lodash";
import TranslateVisitor from "../scilla_parser/translate";
import ScillaTypeChecker from "../scilla_parser/typechecker";
import * as TC from "../scilla_parser/typechecker.js";
import Editor from "@monaco-editor/react";
import util from "util";
import { ppType } from "../scilla_parser/types";

// import antlr4 from 'antlr4';
// import MyGrammarLexer from "../parser/scillaLexer.js";
// import MyGrammarParser from "../parser/scillaParser.js";
const antlr4 = require("antlr4");

const HomePage = (props) => {
  const [code, setCode] = useState("");
  const [parsedCode, setParsedCode] = useState("");

  const parseValue = (literal) => {
    if (literal instanceof Int32L) {
      return `Int32`;
    } else if (literal instanceof Int64L) {
      return `Int64`;
    } else if (literal instanceof Int128L) {
      return `Int128`;
    } else if (literal instanceof Int256L) {
      return `Int256`;
    } else if (literal instanceof Uint32L) {
      return `Uint32`;
    } else if (literal instanceof Uint64L) {
      return `Uint64`;
    } else if (literal instanceof Uint128L) {
      return `Uint128`;
    } else if (literal instanceof Uint256L) {
      return `Uint256`;
    } else if (literal instanceof BNumLit) {
      return `BNum`;
    } else if (literal instanceof StringLit) {
      return `String`;
    } else if (literal instanceof Bystr) {
      return `Bystr`;
    } else if (literal instanceof BystrX) {
      return `BystrX`;
    } else if (literal instanceof Msg) {
      return `Msg`;
    } else if (literal instanceof Map) {
      return `Map`;
    } else if (literal instanceof ADTValue) {
      return `ADTValue`;
    } else if (literal instanceof Clo) {
      return `Clo`;
    } else if (literal instanceof TAbs) {
      return `TAbs`;
    } else {
      return `Undetected literal`;
    }
  };

  const handleRun = () => {
    try {
      resetErrorSettings();
      const input = code;
      const envScillaEvaluator = startingEEnv();
      resetErrorSettings();
      const env = envScillaEvaluator[0];
      const SEEvaluator = envScillaEvaluator[1];
      const chars = new antlr4.InputStream(input);
      const lexer = new scillaLexer(chars);
      const tokens = new antlr4.CommonTokenStream(lexer);
      const parser = new scillaParser(tokens);
      const tree = parser.simple_exp();
      const exprAst = tree.accept(new SyntaxVisitor());

      setPrintTrue();
      resetLogOutput();

      const value = SEEvaluator.evalChildren(exprAst);
      if (isError()) {
        console.log(getError().s);
        setParsedCode(`Error: ${getError().s}`);
      } else {
        console.log("output", value);
        setParsedCode(
          `${parseValue(value)}: ${JSON.stringify(value, null, 4)}`
        );
      }
    } catch (error) {
      setParsedCode("Error: Unrecognisable Syntax");
    }
  };

  const handleTypecheck = () => {
    try {
      resetErrorSettings();
      const tenvSTC = startingTEnv();
      if (isError()) {
        console.log(getError());
      }
      const tenv = tenvSTC[0];
      const STC = tenvSTC[1];
      const input = code;
      // const input = fs.readFileSync("scilexp/church_nat2.scilexp").toString();
      const chars = new antlr4.InputStream(input);
      const lexer = new scillaLexer(chars);
      const tokens = new antlr4.CommonTokenStream(lexer);
      const parser = new scillaParser(tokens);
      const tree = parser.simple_exp();
      const exprAst = tree.accept(new SyntaxVisitor());
      const tenv_ = _.cloneDeep(tenv);
      const typed = STC.typeExpr(exprAst, tenv_);
      if (isError()) {
        console.log(getError());
        setParsedCode(`Error: ${getError().s}`);
      } else {
        console.log(JSON.stringify(util.inspect(typed.ty, false, null, false)));
        setParsedCode(ppType(typed.ty));
      }
    } catch (error) {
      setParsedCode("Error: Unrecognisable Syntax");
    }
  };

  const handleTypecheckCMod = () => {
    try {
      resetErrorSettings();
      const input = code;
      const chars = new antlr4.InputStream(input);
      const lexer = new scillaLexer(chars);
      const tokens = new antlr4.CommonTokenStream(lexer);
      const parser = new scillaParser(tokens);
      const tree = parser.cmodule();
      const cmod = tree.accept(new TranslateVisitor());
      const STC = new ScillaTypeChecker();
      const typed = TC.typeCMod(cmod, {}, STC);
      if (isError()) {
        console.log(getError());
        setParsedCode(`Error: ${getError().s}`);
      } else {
        setParsedCode("true");
      }
      // setParsedCode(`${TC.typeCMod(cmod, {}, STC)}`);
    } catch (error) {
      setParsedCode("Error: Unrecognisable Syntax");
    }
  };

  return (
    <>
      <Flex mb={5}>
        <Button onClick={handleRun} mr={5}>
          Evaluate Expression
        </Button>
        <Button onClick={handleTypecheck} mr={5}>
          Typecheck Expression
        </Button>
        <Button onClick={handleTypecheckCMod}>Typecheck Module</Button>
      </Flex>
      <HStack spacing={8} h="70vh">
        <Editor
          defaultValue={"(* Enter your code here! *)"}
          onChange={(e) => setCode(e)}
          theme="vs-dark"
          h="100%"
        />
        <Textarea value={parsedCode} isReadOnly h="100%" />
      </HStack>
    </>
  );
};

export default HomePage;
