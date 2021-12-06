#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void init(long long* counts, const char* filename) {
    char buffer[1024]; // Sure, why not
    FILE* file = fopen(filename, "r");
    fgets(buffer, 1024, file);
    fclose(file);

    for (char* token = strtok(buffer, ","); token != NULL; token = strtok(NULL, ",")) {
        counts[strtol(token, NULL, 10)] += 1;
    }
}

void step(long long* counts) {
    long long new_counts[9] = {0};

    // Produce new fish
    long long new_fish = counts[0];
    new_counts[8] = new_fish;
    new_counts[6] = new_fish;

    // Shift the rest of the fish timers down
    for (int i = 0; i <= 7; i++) {
        new_counts[i] += counts[i + 1];
    }

    // Copy the new counts back
    memcpy(counts, &new_counts, sizeof(*counts) * 9);
}

long long count_fish(const long long* counts) {
    long long sum = 0;
    for (int i = 0; i <= 8; i++) {
        sum += counts[i];
    }

    return sum;
}

int main() {
    long long counts[9] = {0};
    init(counts, "input.txt");

    for (int i = 0; i < 80; i++) {
        step(counts);
    }

    printf("Part 1: %lld\n", count_fish(counts));

    for (int i = 0; i < (256 - 80); i++) {
        step(counts);
    }

    printf("Part 2: %lld\n", count_fish(counts));

    return 0;
}