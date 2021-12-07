import React, { ChangeEventHandler, useState } from "react";
import {
  Button,
  Stack,
  Heading,
  Textarea,
  Box,
  Text,
  Flex,
} from "@chakra-ui/react";

interface Result {
  alignment: number;
  fuel: number;
}
interface ResultCardProps {
  title: string;
  result: Result;
}

interface ResultsProps {
  titles: string[];
  results: Result[] | undefined;
  ready: boolean;
}

const ResultCard = (props: ResultCardProps) => (
  <Stack
    boxShadow="lg"
    bgColor="blue.500"
    rounded={15}
    p={4}
    w="100%"
    h="100%"
    spacing={2}
  >
    <Heading as="h3" size="sm" color="white" fontWeight={600} opacity={0.7}>
      {props.title}
    </Heading>
    <Heading as="h4" color="white" fontSize="4xl">
      Best alignment: {props.result.alignment}
    </Heading>
    <Text color="white" opacity={0.9} fontWeight={600} fontSize="xl">
      Fuel cost: {props.result.fuel}
    </Text>
  </Stack>
);

const Results = (props: ResultsProps) => {
  if (props.ready) {
    return (
      <Stack w={500} h={400}>
        <ResultCard
          title={props.titles[0]}
          result={props.results![0]}
        ></ResultCard>
        <ResultCard
          title={props.titles[1]}
          result={props.results![1]}
        ></ResultCard>
      </Stack>
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
  const [results, setResults] = useState<Result[]>();

  const calculate = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    const positions = input.split(",").map(parseFloat);

    let minAlignment = Math.min(...positions);
    let maxAlignment = Math.max(...positions);

    // Create an object to store the best alignment and fuel cost for part 1 and 2
    let partOneBest: Result = { alignment: 0, fuel: Infinity };
    let partTwoBest: Result = { alignment: 0, fuel: Infinity };

    // Iterate over each position and calculate the total fuel cost
    for (let i = minAlignment; i <= maxAlignment; i++) {
      var partOneFuel = 0;
      var partTwoFuel = 0;

      for (var pos of positions) {
        const distance = Math.abs(pos - i);

        // Part 1 fuel burn is the distance between the current position and the alignment position
        partOneFuel += distance;

        // Part 2 fuel burn is the sum of positions between the current position and the alignment position
        partTwoFuel += (distance * (distance + 1)) / 2;
      }

      // If the total fuel cost is less than the current best, update the best
      if (partOneFuel < partOneBest.fuel) {
        partOneBest = { alignment: i, fuel: partOneFuel };
      }

      if (partTwoFuel < partTwoBest.fuel) {
        partTwoBest = { alignment, fuel: partTwoFuel };
      }
    }

    // Update the result state
    setResults([partOneBest, partTwoBest]);
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
        <Stack spacing={4}>
          <Heading as="h2" size="md">
            Result
          </Heading>
          <Results
            titles={["Part 1 - Linear Burn", "Part 2 - Non-Linear Burn"]}
            results={results}
            ready={ready}
          />
        </Stack>
      </Stack>
    </Stack>
  );
}

export default App;
