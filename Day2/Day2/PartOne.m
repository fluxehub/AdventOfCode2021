#import "PartOne.h"

@implementation PartOne

NSMutableArray* read_file(NSString* fileName) {
    FILE* file = fopen([fileName UTF8String], "r");
    NSMutableArray* lines = [NSMutableArray array];
    char buffer[64];
    
    while (fgets(buffer, sizeof(char)*64, file) != NULL) {
        NSString* line = [NSString stringWithUTF8String:buffer];
        [lines addObject:line];
    }
    
    return lines;
}

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

+ (void)solution:(NSString*) fileName {
    NSMutableArray* lines = read_file(fileName);
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
