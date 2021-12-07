import React from "react";
import ReactDOM from "react-dom";
import { ChakraProvider, extendTheme } from "@chakra-ui/react";

import App from "./App";

const theme = extendTheme({
  styles: {
    global: {
      "html, body": {
        display: "flex",
        justifyContent: "center",
      },
      "h1, h2, h3, h4, h5, h6, p": {
        opacity: 0.9,
      },
    },
  },
});

ReactDOM.render(
  <React.StrictMode>
    <ChakraProvider theme={theme}>
      <App />
    </ChakraProvider>
  </React.StrictMode>,
  document.getElementById("root")
);
