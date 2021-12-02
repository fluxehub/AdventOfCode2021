#import "PartOne.h"

@implementation PartOne

void translate(NSString* command, int* depth, int* distance) {
    NSArray* commandParts = [command componentsSeparatedByString:@" "];
    NSString* direction = commandParts[0];
    int amount = [commandParts[1] intValue];
    
    // Really? No switch statements for NSStrings????
    if ([direction isEqualToString:@"forward"]) {
        *distance += amount;
    } else if ([direction isEqualToString:@"down"]) {
        *depth += amount;
    } else if ([direction isEqualToString:@"up"]) {
        *depth -= amount;
    } else {
        [NSException raise:@"Invalid command" format:@"Unknown command %@", direction];
    }
}

+ (void)solution:(NSArray*) lines {
    int depth = 0;
    int distance = 0;
    
    for (NSString* line in lines) {
        translate(line, &depth, &distance);
    }
    
    NSLog(@"Final depth: %d", depth);
    NSLog(@"Final distance: %d", distance);
    NSLog(@"Result: %d", distance * depth);
}

@end
