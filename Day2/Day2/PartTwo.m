#import "PartTwo.h"

@implementation PartTwo

void doCommand(NSString* command, int* aim, int* depth, int* distance) {
    NSArray* commandParts = [command componentsSeparatedByString:@" "];
    NSString* action = commandParts[0];
    int amount = [commandParts[1] intValue];
    
    // Really? No switch statements for NSStrings????
    if ([action isEqualToString:@"forward"]) {
        *distance += amount;
        *depth += *aim * amount;
    } else if ([action isEqualToString:@"down"]) {
        *aim += amount;
    } else if ([action isEqualToString:@"up"]) {
        *aim -= amount;
    } else {
        [NSException raise:@"Invalid command" format:@"Unknown command %@", action];
    }
}

+ (void)solution:(NSArray*) lines {
    int aim = 0;
    int depth = 0;
    int distance = 0;
    
    for (NSString* line in lines) {
        doCommand(line, &aim, &depth, &distance);
    }
    
    NSLog(@"Final depth: %d", depth);
    NSLog(@"Final distance: %d", distance);
    NSLog(@"Result: %d", distance * depth);
}

@end

