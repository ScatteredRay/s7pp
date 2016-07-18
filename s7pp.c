#include "s7.h"
#include <stdio.h>
#include <assert.h>

void PrintUsage()
{
    printf("Usage: s7pp <input file>\n");
}

char* GetSource(const char* filename)
{
    FILE* fd = fopen(filename, "r");
    fseek(fd, 0, SEEK_END);
    size_t len = ftell(fd);
    rewind(fd);
    char* source = (char*)malloc(sizeof(char) * (len + 1));
    fread(source, 1, len, fd);
    source[len] = '\0';
    fclose(fd);
    return source;
}

void ReleaseSource(char* source)
{
    free(source);
}

bool IsSymbolChar(char c)
{
    return
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        c == '_' || c == '-';
}

bool IsWhitespace(char c)
{
    return c == ' ' || c == '\t' || c == '\n';
}

size_t PutCNum(const char* c, int n, FILE* out)
{
    for(int i = 0; i < n; i++) {
        fputc(*c++, out);
    }
    return n;
}

size_t ParseString(const char* source, FILE* out)
{
    const char* c = source;
    assert(*c == '"');
    if(out)
        putc(*c, out);
    c++;
    while(*c != '\0') {
        if(*c == '\\') {
            if(out)
                PutCNum(c, 2, out);
            c += 2;
        }
        else if(*c == '"') {
            if(out)
                putc(*c, out);
            c++;
            break;
        }
        else {
            if(out)
                putc(*c, out);
            c++;
        }
    }
    return c - source;
}

void WriteTemplate(s7_pointer ptr, FILE* out, s7_scheme* s7);

// Not const so that we can swap in \0 as an optimzation.
void ParseSource(/*const*/ char* source, FILE* out, s7_scheme* s7)
{
    bool commented;
    bool suppressOutput;
    /*const*/ char* c = source;
    while(*c != '\0') {
        switch(*c) {
        case '\\':
            c += PutCNum(c, 2, out);
            break;
        case '"':
            c += ParseString(c, out);
            break;
        case ';':
            if(*(c+1) != '@')
                putc(*c, out);
            c++;
            break;
        case '@':
            commented = false;
            suppressOutput = false;
            if(c > source && *(c - 1) == ';')
                commented = true;
            c++;
            if(*c == '@') {
                suppressOutput = true;
                c++;
            }
            if(*c == '(') {
                /*const*/ char* exprStart = c;
                c++;
                int level = 1;
                while(level && *c != '\0') {
                    switch(*c) {
                    case '\\':
                        c += 2;
                        break;
                    case '"':
                        c += ParseString(c, (FILE*)0);
                        break;
                    case '(':
                        level++;
                        c++;
                        break;
                    case ')':
                        level--;
                        c++;
                        break;
                    case ';':
                        while(*c != '\n' && *c != '\0')
                            c++;
                        break;
                    default:
                        c++;
                        break;
                    }
                }
                if(!commented) {
                    char tmp = *c;
                    *c = '\0';
                    s7_pointer res = s7_eval_c_string(s7, exprStart);
                    *c = tmp;
                    if(!suppressOutput)
                        WriteTemplate(res, out, s7);
                }
            }
            else if(IsSymbolChar(*c)) {
                /*const*/ char* symStart = c;
                while(IsSymbolChar(*c)) {
                    c++;
                }
                if(!commented) {
                    char tmp = *c;
                    *c = '\0';
                    s7_pointer res = s7_name_to_value(s7, symStart);
                    *c = tmp;
                    if(!suppressOutput)
                        WriteTemplate(res, out, s7);
                }
            }
            else {
                putc('@', out);
                if(suppressOutput)
                    putc('@', out);
            }
            break;
        default:
            putc(*c, out);
            c++;
            break;
        }
    }
}

void WriteTemplate(s7_pointer ptr, FILE* out, s7_scheme* s7)
{
    if(s7_is_string(ptr)) {
        const char* str = s7_string(ptr);
        fputs(str, out);
    }
    else if(s7_is_character(ptr)) {
        char c = s7_character(ptr);
        putc(c, out);
    }
    else if(s7_is_list(s7, ptr)) {
        int n = s7_list_length(s7, ptr);
        for(int i = 0; i < n; i++) {
            s7_pointer ref = s7_list_ref(s7, ptr, i);
            WriteTemplate(ref, out, s7);
            if(i < n - 1) {
                s7_pointer nref = s7_list_ref(s7, ptr, i + 1);
                if(!(s7_is_character(ref) && IsWhitespace(s7_character(ref))) &&
                   !(s7_is_character(nref) && IsWhitespace(s7_character(nref)))) {
                    putc(' ', out);
                }
            }
        }
    }
    else {
        char* str = s7_object_to_c_string(s7, ptr);
        fputs(str, out);
        free(str);
    }
}

int main(int argc, char** argv)
{
    s7_scheme* s7;
    s7 = s7_init();
    if(argc != 2) {
        PrintUsage();
        return 0;
    }
    char* filename = argv[1];
    char* source = GetSource(filename);
    if(!source) {
        PrintUsage();
        return -1;
    }
    //s7_load(s7, argv[1]);
    ParseSource(source, stdout, s7);
    ReleaseSource(source);
    return 0;
}
