import { Box, Button, Container, Flex, HStack, Textarea } from "@chakra-ui/react";
import React, { useState } from "react";

// import antlr4 from 'antlr4';
import MyGrammarLexer from '../parser/scillaLexer.js';
import MyGrammarParser from '../parser/scillaParser.js';
const antlr4 = require('antlr4');

const HomePage = (props) => {
  const [code, setCode] = useState("");
  const [parsedCode, setParsedCode] = useState("");

  const handleRun = () => {
    const input = code;
    const chars = new antlr4.InputStream(input);
    const lexer = new MyGrammarLexer(chars);
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new MyGrammarParser(tokens);
    parser.buildParseTrees = true;
    const tree = parser.simple_exp();
    setParsedCode(tree.toStringTree(parser.ruleNames))
  }

  return (
    <>
    <Flex mb={5}>
      <Button onClick={handleRun}>Run</Button>
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
