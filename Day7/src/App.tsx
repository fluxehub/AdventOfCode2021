import React, { useState } from "react";
import { Stack, Heading, Textarea } from "@chakra-ui/react";
import { useDebouncedCallback } from "use-debounce";

import { Results, Result, State } from "./Results";

function App() {
  const [input, setInput] = useState("");
  const [state, setState] = useState(State.Empty);
  const [results, setResults] = useState<Result[]>();

  const calculate = () => {
    // Mapping parseInt doesn't work, I have to use a lambda
    const positions = input.split(",").map((p) => parseInt(p));

    // If there was an error (i.e. array contains NaN), or if the input contains invalid characters, return an error.
    // Regex is needed because for some reason only known to ECMA parseInt("4aew123", 10) returns 4 instead of NaN.
    if (positions.includes(NaN) || /[^\d,\s]/.test(input)) {
      console.log(positions);
      setState(State.Error);
      return;
    }

    let minAlignment = Math.min(...positions);
    let maxAlignment = Math.max(...positions);

    let bestResults: Result[] = [
      { alignment: 0, fuel: Infinity },
      { alignment: 0, fuel: Infinity },
    ];

    // Iterate over each alignment and calculate the total fuel cost
    for (let i = minAlignment; i <= maxAlignment; i++) {
      var totalFuel = [0, 0];

      for (var pos of positions) {
        const distance = Math.abs(pos - i);

        // Part 1 fuel burn is the distance between the current position and the alignment position
        totalFuel[0] += distance;

        // Part 2 fuel burn is the sum of positions between the current position and the alignment position
        totalFuel[1] += (distance * (distance + 1)) / 2;
      }

      for (let r = 0; r < 2; r++) {
        // If the total fuel cost is less than the current best, update the best
        if (totalFuel[r] < bestResults[r].fuel) {
          bestResults[r] = { alignment: i, fuel: totalFuel[r] };
        }
      }
    }

    // Update the result state
    setResults(bestResults);
    setState(State.Ready);
  };

  // Debounce the input to prevent unnecessary calculations
  const debouncedCalculate = useDebouncedCallback(calculate, 500);

  const changeInput = (e: React.FormEvent<HTMLTextAreaElement>) => {
    setInput(e.currentTarget.value);

    if (e.currentTarget.value.length > 0) {
      setState(State.Pending);
      debouncedCalculate();
    } else {
      setState(State.Empty);
      debouncedCalculate.cancel();
    }
  };

  return (
    <Stack w="100%" p={4}>
      <Heading as="h1" size="3xl" mb={8}>
        Day 7 - Crab Calculator
      </Heading>
      <Stack direction={{ base: "column", xl: "row" }} spacing="24px">
        <Stack>
          <Heading as="h2" size="md" mb={2}>
            Input
          </Heading>
          <Textarea
            w={{ base: "100%", xl: "500px" }}
            h={{ base: "30vh", xl: "400px" }}
            placeholder="Initial crab positions"
            value={input}
            onChange={changeInput}
          />
        </Stack>
        <Stack>
          <Heading as="h2" size="md" mb={2}>
            Result
          </Heading>
          <Results
            titles={["Part 1 - Linear Burn", "Part 2 - Non-Linear Burn"]}
            results={results}
            state={state}
          />
        </Stack>
      </Stack>
    </Stack>
  );
}

export default App;
