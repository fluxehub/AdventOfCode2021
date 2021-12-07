import React, { ChangeEventHandler, useState } from "react";
import { Button, Stack, Heading, Textarea, Box, Text } from "@chakra-ui/react";

interface Result {
  alignment: number;
  fuel: number;
}
interface ResultCardProps {
  result: Result | undefined;
  ready: boolean;
}

const ResultCard = (props: ResultCardProps) => {
  if (props.ready) {
    return (
      <Box w={500} h={400} boxShadow="xl" bgColor="blue.500" rounded={15} p={4}>
        <Text
          as="h3"
          size="sm"
          mb={2}
          color="white"
          fontWeight={600}
          opacity={0.7}
        >
          Submarine Alignment
        </Text>
        <Heading color="white" fontSize="4xl">
          The most optimal horizontal position is {props.result!.alignment}.
        </Heading>
        <Text color="white" fontSize="xl" mt={2} opacity={0.8}>
          This alignment costs {props.result!.fuel} fuel.
        </Text>
      </Box>
    );
  } else {
    return (
      <Box w={500} h={400}>
        <Text as="h3" size="sm" mb={2} fontWeight={600} w={400} opacity={0.3}>
          Result will appear here...
        </Text>
      </Box>
    );
  }
};

function App() {
  const [input, setInput] = useState("");
  const [ready, setReady] = useState(false);
  const [result, setResult] = useState<Result>();

  const calculate = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    const positions = input.split(",").map(parseFloat);

    // Create an object to store the best alignment and fuel cost
    let best: Result = { alignment: 0, fuel: Infinity };

    // Iterate over each position and calculate the total fuel cost
    for (var alignment of positions) {
      var fuel = 0;

      for (var pos of positions) {
        fuel += Math.abs(pos - alignment);
      }

      // If the total fuel cost is less than the current best, update the best
      if (fuel < best.fuel) {
        best = { alignment, fuel };
      }
    }

    // Update the result state
    setResult(best);
    setReady(true);
  };

  const changeInput = (e: React.FormEvent<HTMLTextAreaElement>) => {
    setReady(false);
    setInput(e.currentTarget.value);
  };

  return (
    <Stack>
      <Heading as="h1" size="3xl" mb={8}>
        Day 7 - Crab Calculator
      </Heading>
      <Stack direction={["column", "row"]} spacing="24px">
        <Stack>
          <Heading as="h2" size="md" mb={2}>
            Input
          </Heading>
          <form onSubmit={calculate}>
            <Stack>
              <Textarea
                w={500}
                h={400}
                placeholder="Initial crab positions"
                value={input}
                onChange={changeInput}
              />
              <Button colorScheme="blue" w={24} mt={4} type="submit">
                Calculate
              </Button>
            </Stack>
          </form>
        </Stack>
        <Stack>
          <Heading as="h2" size="md" mb={2}>
            Result
          </Heading>
          <ResultCard result={result} ready={ready} />
        </Stack>
      </Stack>
    </Stack>
  );
}

export default App;
