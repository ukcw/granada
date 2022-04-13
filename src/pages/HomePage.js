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
  resetErrorSettings,
  startingEEnv,
  startingTEnv,
} from "../scilla_parser/general";
import {
  BNumLit,
  Bystr,
  BystrX,
  Int128L,
  Int256L,
  Int32L,
  Int64L,
  Msg,
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

// import antlr4 from 'antlr4';
// import MyGrammarLexer from "../parser/scillaLexer.js";
// import MyGrammarParser from "../parser/scillaParser.js";
const antlr4 = require("antlr4");

const HomePage = (props) => {
  const [code, setCode] = useState("");
  const [parsedCode, setParsedCode] = useState("");
  const parseValue = (literal) => {
    if (literal instanceof Int32L) {
      return `Int32: ${literal.i}`;
    } else if (literal instanceof Int64L) {
      return `Int64: ${literal.i}`;
    } else if (literal instanceof Int128L) {
      return `Int128: ${literal.i}`;
    } else if (literal instanceof Int256L) {
      return `Int256: ${literal.i}`;
    } else if (literal instanceof Uint32L) {
      return `Uint32: ${literal.i}`;
    } else if (literal instanceof Uint64L) {
      return `Uint64: ${literal.i}`;
    } else if (literal instanceof Uint128L) {
      return `Uint128: ${literal.i}`;
    } else if (literal instanceof Uint256L) {
      return `Uint256: ${literal.i}`;
    } else if (literal instanceof BNumLit) {
      return `BNum: ${literal.i}`;
    } else if (literal instanceof Bystr) {
      return `Bystr: ${literal.s}`;
    } else if (literal instanceof BystrX) {
      return `BystrX: ${literal.s}`;
    } else if (literal instanceof Msg) {
      return `BystrX: ${literal.s}`;
    }
  };

  const handleRun = () => {
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
    const value = SEEvaluator.evalChildren(exprAst);
    if (isError()) {
      console.log(getError().s);
      setParsedCode(`${JSON.stringify(getError().s, null, 4)}`);
    } else {
      console.log("output", value);
      setParsedCode(
        `${value.constructor.name}: ${JSON.stringify(value, null, 4)}`
      );
    }
  };

  const handleTypecheck = () => {
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
    }
    // console.log(typed.ty.t.t1);
    setParsedCode(`${JSON.stringify(typed.ty, null, 4)}`);
  };

  const handleTypecheckCMod = () => {
    const input = code;
    const chars = new antlr4.InputStream(input);
    const lexer = new scillaLexer(chars);
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new scillaParser(tokens);
    const tree = parser.cmodule();
    const cmod = tree.accept(new TranslateVisitor());
    const STC = new ScillaTypeChecker();
    setParsedCode(`${JSON.stringify(TC.typeCMod(cmod, {}, STC), null, 4)}`);
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
        <Textarea
          placeholder={"Enter your code here!"}
          onChange={(e) => setCode(e.target.value)}
          h="100%"
        />
        <Textarea value={parsedCode} isReadOnly h="100%" />
      </HStack>
    </>
  );
};

export default HomePage;
