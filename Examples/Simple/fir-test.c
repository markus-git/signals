#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
void main();
void main()
{
    printf("starting...\n");
    FILE* v0;
    FILE* v1;
    float r2;
    float r3;
    float r4;
    float r5;
    float r6;
    float r7;
    float r8;
    float r9;
    float r10;
    float r11;
    float r12;
    float r13;
    float r14;
    float r15;
    float r16;
    float r17;
    float r18;
    float r19;
    float r20;
    float r21;
    float r22;

    v0 = fopen("input", "r+");
    v1 = fopen("output", "r+");
    r2 = 0.0;
    while (true) {
        float v23;
        int v24;

        fscanf(v0, "%f", &v23);
        v24 = feof(v0);
        if (v24) {
            break;
        }
        r10 = v23;
        r17 = 1.0;
        r14 = r5 * r6;
        r21 = 2.0;
        r2 = r9;
        r22 = r2;
        r18 = r7 * r8;
        r11 = r3 + r4;
        fprintf(v1, "%f ", r11);
    }
    fclose(v0);
    fclose(v1);
    printf("ending.\n");
}
