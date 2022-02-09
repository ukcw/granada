import * as React from "react";
import {
  ChakraProvider,
  Box,
  Text,
  Link,
  VStack,
  Code,
  Grid,
  theme,
  Flex,
  Container,
  Button,
} from "@chakra-ui/react";
import { ColorModeSwitcher } from "./ColorModeSwitcher";
import { Logo } from "./Logo";
import HomePage from "./pages/HomePage";

export const App = () => (
  <ChakraProvider theme={theme}>
    <Box textAlign="center" fontSize="xl">
      <Grid minH="100vh" p={3}>
        <ColorModeSwitcher justifySelf="flex-end" />
        <Container maxW={"container.xl"}>
          <Flex mb={5}>
            <Button>Run</Button>
          </Flex>
          <HomePage />
        </Container>
      </Grid>
    </Box>
  </ChakraProvider>
);
