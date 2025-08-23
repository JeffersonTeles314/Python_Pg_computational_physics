#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <conio.h>

#define ROWS 1000
#define COLS 256
#define SCREEN_ROWS 25
#define SCREEN_COLS 80

#define CTRL_KEY(k) ((k) & 0x1f)

char buffer[ROWS][COLS];
int num_lines = 1;

int cx = 0, cy = 0;
int row_offset = 0;

char filename[260] = "sem_nome.txt";

void draw_screen() {
    system("cls");

    for (int i = 0; i < SCREEN_ROWS && (i + row_offset) < num_lines; i++) {
        printf("%.*s\r\n", SCREEN_COLS, buffer[i + row_offset]);
    }

    COORD pos = { cx, cy - row_offset };
    SetConsoleCursorPosition(GetStdHandle(STD_OUTPUT_HANDLE), pos);
}

void insert_char(char c) {
    int len = strlen(buffer[cy]);
    if (len >= COLS - 1) return;
    memmove(&buffer[cy][cx + 1], &buffer[cy][cx], len - cx + 1);
    buffer[cy][cx] = c;
    cx++;
}

void delete_char() {
    if (cx == 0) return;
    int len = strlen(buffer[cy]);
    memmove(&buffer[cy][cx - 1], &buffer[cy][cx], len - cx + 1);
    cx--;
}

void save_file() {
    FILE *fp = fopen(filename, "w");
    if (!fp) return;
    for (int i = 0; i < num_lines; i++) {
        fprintf(fp, "%s\n", buffer[i]);
    }
    fclose(fp);
}

void move_cursor(int key) {
    switch (key) {
        case 72: // up
            if (cy > 0) cy--;
            break;
        case 80: // down
            if (cy < num_lines - 1) cy++;
            break;
        case 75: // left
            if (cx > 0) cx--;
            break;
        case 77: // right
            if (cx < strlen(buffer[cy])) cx++;
            break;
    }

    int len = strlen(buffer[cy]);
    if (cx > len) cx = len;

    if (cy < row_offset) row_offset = cy;
    if (cy >= row_offset + SCREEN_ROWS) row_offset = cy - SCREEN_ROWS + 1;
}

void editor_loop() {
    while (1) {
        draw_screen();

        int ch = _getch();

        if (ch == 0 || ch == 224) {
            int key = _getch();
            move_cursor(key);
        } else if (ch == CTRL_KEY('q')) {
            break;
        } else if (ch == CTRL_KEY('s')) {
            save_file();
        } else if (ch == 13) { // Enter
            if (num_lines < ROWS - 1) {
                memmove(&buffer[cy + 1], &buffer[cy], sizeof(buffer[0]) * (num_lines - cy));
                strcpy(buffer[cy + 1], &buffer[cy][cx]);
                buffer[cy][cx] = '\0';
                cy++;
                cx = 0;
                num_lines++;
            }
        } else if (ch == 8) { // Backspace
            delete_char();
        } else if (ch >= 32 && ch <= 126) {
            insert_char((char)ch);
        }
    }
}
