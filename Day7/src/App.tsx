import { useState } from "react";
import { Button, Stack, Heading, Textarea, Box, Text } from "@chakra-ui/react";

function App() {
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
          <Textarea w={500} h={400} placeholder="Initial crab positions" />
          <Button colorScheme="blue" w={24} mt={4}>
            Calculate
          </Button>
        </Stack>
        <Stack>
          <Heading as="h2" size="md" mb={2}>
            Results
          </Heading>
          <Box
            w={500}
            h={400}
            boxShadow="xl"
            bgColor="blue.500"
            rounded={15}
            p={4}
          >
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
              The most optimal horizontal position is 2.
            </Heading>
            <Text color="white" fontSize="xl" mt={2} opacity={0.8}>
              This alignment costs 37 fuel.
            </Text>
          </Box>
        </Stack>
      </Stack>
    </Stack>
  );
}

export default App;
