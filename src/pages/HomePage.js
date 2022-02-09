import { Box, Container, HStack, Textarea } from "@chakra-ui/react";
import React, { useState } from "react";

const HomePage = (props) => {
  const [code, setCode] = useState("");

  return (
    <HStack spacing={8} h="70vh">
      <Textarea
        placeholder={"Enter your code here!"}
        onChange={(e) => setCode(e.target.value)}
        h="100%"
      />
      <Textarea value={code} isReadOnly h="100%" />
    </HStack>
  );
};

export default HomePage;
