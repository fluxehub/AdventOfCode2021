/// I absolutely despise this language

#import <Foundation/Foundation.h>
#import "PartOne.h"
#import "PartTwo.h"

NSMutableArray* readFile(NSString* fileName) {
    FILE* file = fopen([fileName UTF8String], "r");
    NSMutableArray* lines = [NSMutableArray array];
    char buffer[64];
    
    while (fgets(buffer, sizeof(char)*64, file) != NULL) {
        NSString* line = [NSString stringWithUTF8String:buffer];
        [lines addObject:line];
    }
    
    return lines;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableArray* lines = readFile(@"input.txt");
        
        NSLog(@"Part One:");
        [PartOne solution:lines];
        
        NSLog(@"\nPart Two:");
        [PartTwo solution:lines];
    }
    
    return 0;
}
