#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdint.h>

#define MAX 300
#define MIN 30

char symbols[MAX][MAX];
int v = 0;
            // counter for variables who has simple assignment, i.e., a = b, b = 1
int declared = 0;
//function declaration

//#region
int isNumber(const char *);
int is_binary_string(char *);
void removeSpace(char *);
int isOperator(char);
int redeclaration(char [][MAX], int);
int noOperator(char *);
int contextualChecker(char *, int dec, char *);
int variableChecker(char [][MAX], int, char [][MIN][MIN], int [], int*, int*);

void repetitiveComma(char *, int *);
int specialCase(char *);
int compoundChecker(char *, int *);
int tokenizer(int , char [][MAX], char* );
int precedence(char);
void push(char);
char pop();
void push_int(int);

int pop_int();
int parenthesisChecker(char [], char [][MAX], int);

int mathematical_expression(char[][MAX], int *, int );

int syntax_analyzer(char [][MAX], int , char [][MAX], int *, char [][MAX], char [][MIN][MIN], char [][MIN], int[], int*, int*);
//#endregion

int isNumber(const char *str) {
    if (str == NULL || *str == '\0')
        return 0;  // empty string not a number

    for (int i = 0; str[i] != '\0'; i++) {
        if (!isdigit(str[i]))
            return 0; // if any char is not digit → false
    }
    return 1; // all digits → true
}

int is_binary_string(char *str){
    if (str == NULL || *str == '\0') {
        // An empty or NULL string could be considered invalid or valid,
        // depending on requirements. Here, we'll treat it as false.
        return 0;
    }

    for (int i = 0; str[i] != '\0'; i++) {
        if(isspace(str[i])){
            continue;
        }
        if (str[i] != '0' && str[i] != '1') {
            return 0; // Found a non-binary character
        }
    }
    return 1; // All characters checked and passed
}
void removeSpace(char *str){
    char result[MAX];

    int j = 0, isbinary = is_binary_string(str);                    //binary string checker
    int len = strlen(str);

    for(int i = 0; i < len; i++){
        
        if(i == 0 && isspace(str[i]))                               // skips leading spaces (" int a;" " a = a + b;")
            continue;
        
        if(isspace(str[i])){
            
            /*
                keeps single space between strings i.e., "variable name"
                result should be token[1] = "variable name" instead of token[1] = "variable" token[2] = "name"

                cons: i.e., "int a;" --> token[1] = "int a"
            */
            if(isalnum(str[i - 1]) && isalnum(str[i + 1])){         
                result[j++] = ' ';                                  
            }

        }else if(strchr("=,+*/;", str[i])){
                                                        
            if(j > 0 && result[j - 1] == ' ')                       // removes previous space before operator
                j--;                                                // "a + b" --> "a+ b"
            
            result[j++] = str[i];                                   // a+

            while(isspace(str[i + 1]))                              // skips spaces after operator
                i++;
        }else{
            result[j++] = str[i];    
        }
        
    }

    while(j > 0 && isspace(result[j - 1]))
        j--;
    
    result[j] = '\0';
    strcpy(str, result);
}
int isOperator(char symbol) {
    switch (symbol) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '^':
        case '(':
        case ')': return 1;
        default: return 0;
    }
}
int redeclaration(char tokens[][MAX], int tokcount){
    int repetition = 0;
    
    for(int i = 2; i < tokcount; i++){
        char *str = tokens[i];
        for(int j = 2; j < tokcount; j++){
            char *comp = tokens[j];
            if(strcmp(str, ",") == 0){
                continue;
            }else if(strcmp(str, comp) == 0){
                repetition++;
                //printf("str [%s] comp [%s] count = %d\n", str, comp, repetition);
            }
        }
        if(repetition > 1){
            printf("*ERROR: redeclaration of '%s'*\n", str);
            return 0;
        }
        repetition = 0;
    }
    //success
    return 1;
}

int noOperator(char *str){
    if(strstr(str, "+") == NULL && strstr(str, "-") == NULL &&
       strstr(str, "*") == NULL && strstr(str, "/") == NULL ){
        return 0;
    }
    return 1;
}
int contextualChecker(char *temp, int dec, char *variable){
    int len = strlen(temp);
    int validity = 0;
    
    if(strstr(temp, "=") != NULL){  //this line means that the assigned value has not been separated from its "=" and identifier --> still needs further processing.
        return 1;
    }
    if(isNumber(temp)==1){
        return 1;
    }else{
        //extract variables in expression like a + b + 1
        
        char var[MAX][MAX];
        int count = 0, varCount = 0;
        
        for(int i = 0; i < len; i++){
            
            if(isdigit(temp[i])!=0){
                if(count > 0){
                    var[varCount][count++] = temp[i];
                }
                continue;
            }

            if(temp[i] == ','){
                continue;
            }
            if(isOperator(temp[i]) == 1){
                if(count > 0){
                    var[varCount][count] = '\0';
                    varCount++;
                    count = 0;
                }
                continue;
            }

            if(isalpha(temp[i])){
                var[varCount][count++] = temp[i];
            }
        }
        if(count > 0){
            var[varCount][count] = '\0';
            varCount++;
        }


        /*
        This loop checks if the expression uses identifiers that has been declared.
        i.e., int a, b, c;
        b = a + b;          >> be uses declared variables a & b
                            >> this is correct
        i.e., b = a + c + d >> it uses undeclared variable d
                            >> this is invalid
        */
        int ct = dec;
        if(variable == " "){
            ct = v;
        }
        
        
        
        for(int i = 0; i < varCount; i++){                  // checks if variables used in mathematical expression is declared or not
            
            for(int j = 0; j < ct; j++){               // declared counter is supposed to be for flagging the number of declarations at runtime
                if(strcmp(symbols[j], var[i]) == 0){
                    validity++;
                }
            }

        }
        
        
        if(validity == varCount){                           // varCount >> number of variables in an expression
                                                            // validity >> all variables in an expression are valid
                                                            // validity == varcount >> should always be equal. otherwise, it is invalid
            return 1;
        }

        
        return 0;
    }
}

int variableChecker(char tokens[][MAX], int tokcount, char varVal[][MIN][MIN], int toggle[], int *counter_t, int* varValcnt){
    int i = 0, j = 0, hasInt = 0;

    
    if(strcmp(tokens[0], "int") == 0){                  // if string has "int", it sets counter to 1
        i = 1;
        hasInt = 1;
    }
    int toggle_temp = *counter_t;
    int b = *varValcnt;
    for(i; i < tokcount; i++){   
        
        if(strcmp(tokens[i], ",") == 0 || strcmp(tokens[i], ";") == 0){continue;}

        
        char *p = strchr(tokens[i], '=');
        if(p != NULL){
            //has assignment
            int pos = p - tokens[i];
            char *value = p+1;

            char var[MAX];
            strncpy(var, tokens[i], pos);
            var[pos] = '\0';
            
            int noOps = noOperator(value);
            if(noOps == 0){
                strcpy(varVal[b][0], var);
                strcpy(varVal[b][1], value);
                toggle[toggle_temp++] = 4;
                b++;
            }
            
            //checks if first character is digit
            if(var[0] == ' '){
                if(isdigit(var[1])!=0){
                    printf("*ERROR: invalid suffix on integer constant\n*"); return 0;
                }
                removeSpace(var);
                if(strchr(var, ' ')!=NULL){printf("*ERROR: expected '=', ',', ';', 'asm' or '__attribute__'"); 
                    return 0;
                }
            }else{
                if(strchr(var, ' ')!=NULL){printf("*ERROR: expected '=', ',', ';', 'asm' or '__attribute__'*"); 
                    return 0;
                }
            }
            if(isdigit(var[0])!=0){printf("*ERROR: invalid suffix on integer constant\n*"); 
                return 0;
                }
            if(strchr(var, '-')!=NULL){printf("*ERROR: expected '=', ',', ';', 'asm' or '__attribute__'*"); 
                return 0;
                }
            if(strchr(var, '#')!=NULL){printf("*ERROR: stray '#' in program*"); 
                return 0;
            }
            if(strchr(var, '@')!=NULL){printf("*ERROR: stray '@' in program*"); 
                return 0;
            }
            if(strstr(var, "int ")!=NULL){printf("*ERROR expected '=', ',', ';', 'asm' or '__attribute__'*"); 
                return 0;
            }
            if(strstr(var, "char")!=NULL){printf("*ERROR: expected identifier or '(' before 'char'*"); 
                return 0;
            }
            if(strstr(var, "while ")!=NULL){printf("*ERROR: expected identifier or '(' before 'while'*"); 
                return 0;
            }

            // if the expression has "int" then variable is declared and should be stored as checker
            if(hasInt == 1){
                removeSpace(var);
                strcpy(symbols[v], var);
                v++;
            }
        }else{
            
            if(hasInt == 1){
                removeSpace(tokens[i]);
                strcpy(symbols[v], tokens[i]);
                v++;
            }else{ 
                //expressions with no declaration and assignment
                return 2;
                
            }
            
        }
        
    }
    //success
    *varValcnt = b;
    *counter_t = toggle_temp;
    return 1;
}




void repetitiveComma(char *string, int *comma){
    int count = 0, checker = 0;
    for(int i = 0; string[i] != '\0'; i++){
        if(string[i] == ','){
            count++;
        }
        if(string[i] == ',' && string[i+1] == ','){
            checker++;
        }
    }

    *comma = checker;
}

int specialCase(char *string){
    char *temp = string;

    // int a;;
    // int b, a;;
    
        if(strstr(temp, ";")!=NULL){
            //check first occurrence of ;
            //extract the rest of the string
            //check for a comma --> error
            //if no comma -->extract string before ;
            char *p = strchr(temp, ';');

            if (p != NULL) {
                char *checker = p + 1;

                //string after first occurence of ; is checked
                // int a;,;b;    --> ;b; (gets checked)
                if(strchr(checker, ',')!=NULL){
                    printf("*ERROR: expected expression before ',' token*\n");
                    return 0;
                }
                int new = strlen(temp);
                
                int len = p - temp;
                char before[MAX];
                strncpy(before, temp, len);
                before[len] = '\0';
                
                // int a,;;
                
                if(before[len-1] == ','){
                    printf("*ERROR: expected identifier or '(' before ','*\n");
                }

                before[len++] = ';';
                before[len++] = '\0';
                strncpy(string, before, len);
                string[len-1] = '\0';
                
            }
        }
    

    return 1;

}

int compoundChecker(char *string, int *checker){
    char *temp = string;
    char tem[MAX];
    strcpy(tem, string);
    
    char *plus = "+=";
    char *minus = "-=";
    char *times = "*=";
    char *divide = "/=";
    char statement[MAX] = ""; // buffer to store all reconstructed statements
    
   

    if(strstr(temp, "int") != NULL){
        return 1;  // simple type declaration
    } else if(strstr(temp, plus) != NULL || strstr(temp, minus) != NULL ||
              strstr(temp, times) != NULL || strstr(temp, divide) != NULL){

        char *token = strtok(tem, ",;");
        while(token != NULL){
            while(*token == ' ') token++; // remove leading spaces

            char *operator_ = NULL;
            char *op = NULL;

            if ((op = strstr(token, plus)) != NULL) operator_ = "+";
            else if ((op = strstr(token, minus)) != NULL) operator_ = "-";
            else if ((op = strstr(token, times)) != NULL) operator_ = "*";
            else if ((op = strstr(token, divide)) != NULL) operator_ = "/";

            if(op && operator_){
                *op = '\0'; // split variable and value
                char *var = token;
                op += 2; // skip past operator

                while(*op == ' ') op++; // skip leading spaces
                char *val = op;

                char tempStatement[100];
                snprintf(tempStatement, sizeof(tempStatement), "%s=%s%s%s", var, var, operator_, val);

                if(statement[0] != '\0') strcat(statement, ",");
                strcat(statement, tempStatement);
            }

            token = strtok(NULL, ",;"); // move to next token
        }

        int len = strlen(statement);
        statement[len++] = ';';
        statement[len++] = '\0';
        strcpy(string, statement);
        
        *checker = 1;
        return 1; // compound statement found
    }else{
        return 1;
    }

    return 0; // nothing found
}

int unaryChecker(char *string, int *checker){
    
    if(strstr(string, "++")!=NULL || strstr(string, "--")!=NULL){
        printf("\n*ERROR: Statement uses unary operator*\n");
    }
    return 0;
}
int tokenizer(int type, char tokens[][MAX], char*string){
    /*
        tokenizer is both initial checker of the syntax and at the same time tokenizes the string
    
    */

    
   int i = 0, j = 0, tokCount = 0, checker = 0;
   char c;
    switch(type){
        case 1:                                                     // code textfile tokenizer
    
            removeSpace(string);

            checker = specialCase(string);
            
            if(checker == 0){
                return 0;
            }
            //checking for possible int a;;;
            repetitiveComma(string, &checker);
            
            if(checker > 0){
                printf("*ERROR: expected identifier or '(' before ','*\n");
                return 0;
            }

            int failing = 0;
            checker = compoundChecker(string, &failing);
            
            if(checker == 0){
                printf("*ERROR*\n");
                return 0;
            }

            
            char str[MAX];
            
            int hasTerminal = 0;
            
            if(strstr(string, ";") != NULL){
                hasTerminal++;
            }
            
            int len = strlen(string);
            if((string[0] == 'i' || string[0] == 'I') &&            // checks if string starts on "int"
            (string[1] == 'n' || string[1] == 'N') &&               
            (string[2] == 't' || string[2] == 'T')){
                
                char temp[4];
                strncpy(temp, string, 3);
                temp[3] = '\0';
                if(strcmp(temp, "int") != 0){                       // it makes sure that it should be "int" and not "INT" or any wrong data type
                    printf("*ERROR*\n"); 
                    return 0;
                }else{
                    i = 1;                                          // it sets i to 1, flagging that there's int
                    tokCount = 1;
                    declared++;
                }
            }

            if((string[0] == 'c' || string[0] == 'C') && 
            (string[1] == 'h' || string[1] == 'H') && 
            (string[2] == 'a' || string[2] == 'A') &&
            (string[3] == 'r' || string[3] == 'R')){

                
                char temp[4];
                strncpy(temp, string, 3);
                temp[3] = '\0';
                if(strcmp(temp, "char") != 0){
                    printf("*ERROR: UNKNOWN TYPE NAME %c%c%c%c*\n", string[0], string[1], string[2], string[3]); 
                    exit(0);
                }else{
                    printf("*CODE DOESN'T ACCEPT CHAR DECLARATION*\n");
                    exit(0);
                }
            }

            if((string[0] == 'f' || string[0] == 'F') && 
            (string[1] == 'l' || string[1] == 'L') && 
            (string[2] == 'o' || string[2] == 'O') &&
            (string[3] == 'a' || string[3] == 'A') &&
            (string[4] == 't' || string[4] == 'T')){

                
                char temp[4];
                strncpy(temp, string, 3);
                temp[3] = '\0';
                if(strcmp(temp, "int") != 0){
                    printf("*ERROR: UNKNOWN TYPE NAME %c%c%c%c%c*\n", string[0], string[1], string[2], string[3]); 
                    return 0;
                }else{
                    printf("*CODE DOESN'T ACCEPT FLOAT DECLARATION*\n");
                    return 0;
                }
            }


            if(i == 1){                                            // if int flag is 1, then it initially gets stored as the first token
                //store first term
                strncpy(tokens[0], "int", MAX);
                //extract rest of the string
                strcpy(str, string+3);
            }else{
                strcpy(str, string);                               // str is a temporary holder for string and is used in the loop
            }
            if(strstr(str, "int") != NULL){                        // it flags error if there's a repeatitive "int" in the string
                if(strstr(str, "int ") != NULL){            
                    printf("*ERROR: two or more data types in declaration specifiers*");
                    return 0;
                }else{
                    printf("*ERROR: expected identifier or '(' before 'int'*");                              // expected identifier or '(' before 'int'|
                    return 0;
                }
            }

            /*
                If the string passes the initial checking of the syntax then it proceeds on 
                tokenizing the rest of the string.
            */
            j = 0;
            for(i = 0; (c = str[i]) != '\0'; i++){
                
                if(c == ',' || c == ';'){
                    if( j > 0){
                        tokens[tokCount][j] = '\0';
                        tokCount++;
                        j = 0;
                    }
                    //store the delimiter as a token
                    tokens[tokCount][0] = c;
                    tokens[tokCount][1] = '\0';
                    tokCount++;
                }else{
                    tokens[tokCount][j++] = c;
                }
            }
            if(j > 0){
                tokens[tokCount][j] = '\0';
                tokCount++;
            }

            if(hasTerminal == 0){
                printf("ERROR: expected '=', ',', ';'*\n");
                return 0;
            }

            return tokCount;
            break;
        case 2:
           

            while ((c = string[i++]) != '\0') {

                // Handle delimiters
                if (c == ',' || c == ';' || c == ' ' || c == '(' || c == ')') {
                    // Save previous token if any

                    if (j > 0) {
                        tokens[tokCount][j] = '\0';
                        tokCount++;
                        j = 0;
                    }

                    
                }
                else {
                    // Normal character (part of a token)
                    if (j < MAX - 1) {
                        tokens[tokCount][j++] = c;
                    } else {
                        printf("Warning: token too long, truncating.\n");
                    }
                }
            }

            // Save the last token if any
            if (j > 0) {
                tokens[tokCount][j] = '\0';
                tokCount++;
            }
            
            
            return tokCount;
            break;
        default:
            break;
    }
}

//operators stack
char stack[MAX];
int top = -1;

//integer stack
int stack_int[MAX];
int top_int = -1;

int precedence(char symbol) {
    switch (symbol) {
        case '+':
        case '-': return 2;
        case '*':
        case '/': return 3;
        case '^': return 4;
        case '(':
        case ')':
        case '#': return 1;
        default: return 0;
    }
}
// Push for char stack
void push(char item) {
    if (top >= MAX - 1) {
        printf("Error: Stack overflow while pushing '%c'.\n", item);
        return;
    }
    stack[++top] = item;
}
// Pop for char stack
char pop() {
    if (top < 0) {
        printf("Error: Stack underflow during pop.\n");
        return '#';
    }
    return stack[top--];
}
// Push for int stack
void push_int(int item) {
    if (top_int >= MAX - 1) {
        printf("Error: Integer stack overflow.\n");
        return;
    }
    stack_int[++top_int] = item;
}
// Pop for int stack
int pop_int() {
    if (top_int < 0) {
        printf("Error: Integer stack underflow.\n");
        return 0;
    }
    return stack_int[top_int--];
}

int parenthesisChecker(char infix[], char postfix[][MAX], int numPF){
    int i = 0, j = 0;
    int l = numPF;
    stack[++top] = '#';
    
    // sample: (a+b)c --> this only gets an error when postfix enters mathematical expression
    /*
        reads '('   >>  push '('
                    >>  stack['#', '('] --> top = 1
        reads 'a'   >>  postfix[0][0] = a
                    >>  postfix[0][1] = ' '
                    >>  j --> 2
        reads '+'   >>  + has GREATER precedence than '(' (doesn't enter condition), pushes '+'
                    >>  stack['#', '(', '+'] --> top = 2
        reads 'b'   >>  postfix[0][2] = 'b'
                    >>  postfix[0][3] = ' '
                    >>  j --> 4
        reads ')'   >>  pops until '('
                    >>  postfix[0][4] = '+'
                    >>  postfix[0][5] = ' '
                    >>  stack['#', '('], exits while loop
                    >> enters condition --> if (stack[top] == '(')
                        >> pops '('
                        >> stack['#'] --> top = 0
        reads 'c'   >> postfix[0][6] = 'c'
                    >> postfix[0][7] = ' '
        
        exits first while loop
        doesn't enter second loop because stack['#']
        postfix[0][8] = '\0'

    */

    // sample: (a+b)*(c+d)
    /*
        reads '('   >> push '('
                    >> stack['#', '('] --> top = 1
        reads 'a'   >> postfix[0][0] = 'a'
                    >> postfix[0][1] = ' '
                    >> j --> 2
        reads '+'   >> '+' has GREATER precedence than '(' → push '+'
                    >> stack['#', '(', '+'] --> top = 2
        reads 'b'   >> postfix[0][2] = 'b'
                    >> postfix[0][3] = ' '
                    >> j --> 4
        reads ')'   >> pops until '('
                    >> postfix[0][4] = '+'
                    >> postfix[0][5] = ' '
                    >> j --> 6
                    >> stack['#', '('], exits while loop
                    >> enters condition --> if (stack[top] == '(')
                        >> pops '('
                        >> stack['#'] --> top = 0
        reads '*'   >> * has GREATER precedence than '#' (doesn't enter condition), pushes *
                    >> stack['#', '*']
        reads '('   >> push '('
                    >> stack['#', '*', '(']
        reads 'c'   >> postfix[0][6] = 'c'
                    >> postfix[0][7] = ' '
                    >> j --> 8
        reads '+'   >> '+' has GREATER precedence than '(', pushes '+'
                    >> stack['#', '*', '(', '+']
        reads 'd'   >> postfix[0][8] = 'd'
                    >> postfix[0][9] = ' '
                    >> j --> 10
        reads ')'   >> pops until '('
                    >> postfix[0][10] = '+'
                    >> postfix[0][11] = ' '
                    >> stack['#', '*', '('], exits while loop
                    >> enters condition --> if (stack[top] == '(')
                        >> pops '('
        exits first while loop
        enters another while loop
            stack['#', '*']
            postfix[0][12] = '*'
            postfix[0][13] = ' '
            exits while loop
        postfix[0][14] = '\0'

        postfix would be "a b + c d + *"

    */

    while (infix[i] != '\0') {
        if (isspace(infix[i])) {
            i++;
            continue;
        }

        // If it's a variable or number (operand)
        if (isalnum(infix[i])) {
            while (isalnum(infix[i])) {
                postfix[l][j++] = infix[i++];
            }
            postfix[l][j++] = ' ';  // space separates operands
        } 

        // opening parenthesis
        else if (infix[i] == '(') {
            push(infix[i]);
            i++;
            continue;
        } 

        //closing parenthesis
        else if (infix[i] == ')') {

            if(top == -1 || stack[top] == '#'){
                printf("*ERROR: expected expression before ')' token ')'*\n");
                return 0;
            }

            while (stack[top] != '(' && top >= 0) {
                postfix[l][j++] = pop();
                postfix[l][j++] = ' ';

                    if(top == -1 || stack[top] == '#'){
                    printf("*ERROR: expected ',' or ';' before ')' token *\n");
                    return 0;
                    }

            }
            if (stack[top] == '(')
                pop();
            else {
                printf("*ERROR: expected ',' or ';' before ')' token *\n");
                return 0;
            }
            i++;
        } 

        // Operator
        else if (isOperator(infix[i])) {
            while (precedence(infix[i]) <= precedence(stack[top]) && top >= 0) {
                postfix[l][j++] = pop();
                postfix[l][j++] = ' ';
            }
            push(infix[i]);
            i++;
        } 
        else {
            printf("*ERROR: expected ',' or ';' before ')' token *\n");
            return 0;
        }
    }

    while (stack[top] != '#') {
        if(stack[top] == '('){
            printf("*ERROR: expected ')' before ';' token *\n");
            return 0;
        }
        postfix[l][j++] = pop();
        postfix[l][j++] = ' ';
    }
    postfix[l][j] = '\0';
    
    return 1;
}

int mathematical_expression(char postfix[][MAX], int *error, int numPF) {
    
    char stack[MAX][MAX];   // stack of string operands
    char token[MAX];
    int top = -1;
    int i = 0;
    int l = numPF;
    *error = 0;

    //sample "a b + c d + *"
    /*
        reads 'a'   >>  stack['a']
                    >>  top = 1
        reads 'b'   >>  stack['a', 'b']
                    >>  top = 2
        reads '+'   >>  top = 0
                    >>  stack['X']
                    >>  top = 1
        reads 'c'   >>  stack['X', 'c']
                    >>  top = 2
        reads 'd'   >>  stack['X', 'c', 'd']
                    >>  top = 3
        reads '+'   >>  top = 1
                    >>  stack['X', 'X']
                    >>  top = 2
        reads '*'   >>  top = 0
                    >>  the 2 X's in stack gets popped
                    >>  pushes X as the result of the two expressions
                    >>  stack['X']
                    >>  top = 1
    */
    //sample "a+"
    /*
        reads 'a'   >> stack['a']
                    >> top = 1
        reads '+'   >> error 
    */
    while (postfix[l][i] != '\0') {

        // skip whitespace
        if (isspace(postfix[l][i])) {
            i++;
            continue;
        }

        // --- identifiers or integers---
        else if (isalpha(postfix[l][i]) || isdigit(postfix[l][i])) {
            int k = 0;
            while (isalnum(postfix[l][i])) {
                token[k++] = postfix[l][i++];
            }
            token[k] = '\0';
            strcpy(stack[++top], token);   // pushes operand
            continue;
        }

        // --- operators ---
        else if (isOperator(postfix[l][i])) {

            // need 2 operand
            /*
                stack should at least have 2 operands.
            */
            if (top < 1) {
                *error = 1;
                return 0;
            }

            // pop 2 operands
            top--;  // pop right operand
            top--;  // pop left operand
            

            // push a placeholder to represent the result
            strcpy(stack[++top], "X");

            i++;
            continue;
        }

        // invalid token
        else {
            *error = 1;
            return 0;
            /*
                it gets in here if postfix is "a b + c" --> "(a+b)c"
                there should only be two operands
            */
        }
    }

    // at end of expression, only 1 item must remain
    if (top != 0) {
        *error = 1;
        return 0;
    }

    return 1;  // valid
}



int syntax_analyzer(char tokens[][MAX], int tokCount, char postfix[][MAX], int *fail, char varTemp[][MAX], char varVal[][MIN][MIN], char constantExpression[][MIN], int toggle[], int *counter_t, int *varValCnt){
    int check = 1, i = 0, hasInt = 0, var = 0, error = 0, numPF = 0, b = *varValCnt;
    char infix[MAX];
    
    check = redeclaration(tokens, tokCount);
    
    if(check == 0){
        
        *fail = 1;
        return 0;
    }
  
    check = variableChecker(tokens, tokCount, varVal, toggle, counter_t, varValCnt); // assigned value in the int line expression is added successfully
    if(check == 0){
        *fail = 1;
        return 0;
    }

    
    
    int toggle_temp_counter = 0;
    int noOPexpress = 0;
    int dec = 0;

    if(strcmp(tokens[0], "int") == 0){
        i = 1;
        hasInt = 1;
        var++;
        dec = declared;
    }else{
        i = 0;
        hasInt = 0;
        dec = v;
    }

    if(check == 2){
        //expression has no assignment and declaration
        for(int j = 0; j < tokCount; j++){
            char *str = tokens[j];
            char *noVar = " ";

            if(strcmp(str, ";")==0){
                continue;
            }
            if(strcmp(str, ",")==0){
                continue;
            }
                // this will be used as a counter area
            //compoundChecker(char *string, int *checker)
            printf("\nsyntax checking str = %s\n", str);
            int success = contextualChecker(str, declared, noVar);
            
            int unaCheck = 0;
            unaryChecker(str, &unaCheck);
                if(unaCheck != 0){
                    return 0;
                }
            // the variables enside each expresions are valid
            if(success == 1){
                 printf("\nI'm here = %s\n", str);
                 //can be further processed
                if(strstr(str, "+") == NULL && strstr(str, "-") == NULL && strstr(str, "*")==NULL && strstr(str, "/") == NULL && strstr(str, "=") == NULL){
                    
                    toggle[toggle_temp_counter] = 1; // with no operators
                    if(strstr(str, ")") != NULL && strstr(str, "(") != NULL){
                        strcpy(infix, str);
                        int c = 1;
                        c = parenthesisChecker(infix, postfix, numPF);
                        
                        if(c == 0){
                            *fail = 1;
                            return 0;
                        }
                        if(c == 1){
                            
                            char *word = postfix[toggle_temp_counter];
                            removeSpace(word);
                            // strcpy(constantExpression, word);
                            strcpy(constantExpression[noOPexpress++], word); //newly added
                            
                            strcpy(postfix[toggle_temp_counter], "\0");
                            
                        }

                    }else{
                        // needs to generate mips that loads this
                        
                        //strcpy(constantExpression, str);
                        printf("\nI am here = %s\n", str);
                        strcpy(constantExpression[noOPexpress++], str);
                        
                    }
                }else if(strstr(str, "=") != NULL){
                    //printf("\nEnters this processor\n");
                    char *p = strchr(str, '=');
                    
                    int pos = p - str;                       // position after "="
                    char *temp = p+1;                        // temp has expression after "="
                    printf("equal sign = %s\n", temp);
                    char variable[MAX];
                    strncpy(variable, str, pos);
                    variable[pos] = '\0';
                    
                    // contextual checking is no longer necessary since it has already been done for the entire line

                    if(strstr(temp, "+") != NULL && strstr(temp, "-") != NULL && strstr(temp, "*") != NULL && strstr(temp, "/")!=NULL){
                        printf("Operation = %s\n");
                        strcpy(varTemp[numPF], variable);
                        strcpy(infix, temp);
                    
                        check = parenthesisChecker(infix, postfix, numPF);
                        if(check == 0){
                            *fail = 1;
                            return 0;
                        }
                        
                        check = mathematical_expression(postfix, &error, numPF);
                        if(check == 0 || error == 1){
                            *fail = 1;
                            return 0;
                        }
                        toggle[toggle_temp_counter] = 3;
                        numPF++;
                    }else{
                        
                        strcpy(varVal[b][0], variable);
                        strcpy(varVal[b][1], temp);
                        printf("val[%s]var[%s]\n", varVal[b][1], varVal[b][0]);
                        b++;
                        toggle[toggle_temp_counter] = 4;
                        continue;
                    }
                }
                else{
                        // with operators
                        
                        strcpy(infix, str);
                    
                        /*
                            create a toggle
                            store expressions with no operators in a 2D array
                            1 means no operators
                            2 means with operators
                            the counter of the for loop uses line 993
                                >> this means the number of expressions in one line
                            inside the loop there are two conditions
                                >> if the first index of the toggle is one
                                >> it enters condition with no operators for translation
                                    >> its index will get incremented
                                >> if the next index of the toggle is two 
                                >> it enters condition with postfix for translation
                                    >>its index will get incremented
                            
                            for(int i = 0; i < counter; i++){
                                int temp = toggle[i];
                                if(temp == 1){
                                    postfix[l++] to translate
                                }
                                if(temp == 2){
                                    with no operators[k++] to translate
                                }
                            }
                            >>this is to ensure that the order of expressions within the line are
                            translated orderly
                        */
                        check = parenthesisChecker(infix, postfix, numPF);
                            if(check == 0){
                                *fail = 1; 
                                return 0;
                            }
                            
                            check = mathematical_expression(postfix, &error, numPF);
                            if(check == 0 || error == 1){
                                *fail = 1;
                                return 0;
                            }
                        
                            
                        numPF++;
                        toggle[toggle_temp_counter] = 2;
                }

            }
            toggle_temp_counter++;

            //variables being used in each expression are invalid
            if(success == 0){
                printf("\n*ERROR: undeclared variable*\n");
                return 0;
            }
        }
    } // this is the end for this line


    else{
        //printf("\nI AM HERE\n");
        if(tokens[0] == ";"){
                return 0;
        }
        for( i; i < tokCount; i++){
            removeSpace(tokens[i]);
            
            if(strstr(tokens[i], "+")==NULL && strstr(tokens[i], "-") == NULL &&
            strstr(tokens[i], "*")==NULL && strstr(tokens[i], "/") == NULL && strstr(tokens[i], "=") == NULL) {
                    continue;
            }
            printf("\nsyntax checking str = %s\n", tokens[i]);
            //printf("[%d][%s]\n", i, tokens[i]);
            char *p = strchr(tokens[i], '=');

            if(p != NULL){
                int pos = p - tokens[i];                       // position after "="
                char *temp = p+1;                              // temp has expression after "="
                
                
                char variable[MAX];
                strncpy(variable, tokens[i], pos);
                variable[pos] = '\0';
                
                check = contextualChecker(temp, dec, variable);
                
                if(check == 0){
                    printf("*ERROR: undeclared identifier*\n");
                    *fail = 1;
                    return 0;
                }else{
                    
                    strcpy(varTemp[numPF], variable);
                    strcpy(infix, temp);
                    //printf("Processing here! [%d][%s]\n", i, infix);
                    if(strstr(temp, "+") != NULL || strstr(temp, "-") != NULL || strstr(temp, "*") != NULL || strstr(temp, "/") != NULL){
                        //expression with a mathematical expression
                        //printf("With operators = %s", temp);
                        check = parenthesisChecker(infix, postfix, numPF);
                        if(check == 0){
                            *fail = 1;
                            return 0;
                        }
                        
                        check = mathematical_expression(postfix, &error, numPF);
                        if(check == 0 || error == 1){
                            *fail = 1;
                            return 0;
                        }
                        toggle[toggle_temp_counter++] = 5;
                        numPF++;
                    }else{
                        strcpy(varVal[b][0], variable);
                        strcpy(varVal[b][1], temp);
                        b++;
                        toggle[toggle_temp_counter++] = 4; // temporary
                    }
                }

            }
        }
    
       
    }
    
    /*
        the rest of the code of this function only operates when it encounters lines like:
        a = b;
        int a; int a=10, b;
    */
    *varValCnt = b;
    *counter_t = toggle_temp_counter;
    // for(int i = 0; i < toggle_temp_counter; i++){
    //     printf("[%d][%d]\n", i, toggle[i]);
    // }
    return numPF;
    
}

void intermediate_code(char *postfix, char *variable, FILE *initialEdumips, char *constantExpression, int type){
    char stack[100][20];
    int top = -1;
    int temp = 1;

    char token[20];
    char result[20];
    int i = 0;

    /*
        Register Management:
            R1, R2 are used for loading operands
            R3-R8 are used for storing intermediate results
    */
    int temp_reg_id = 3;
    const int MAX_TEMP_REG = 8;
    const int START_TEMP_REG = 3;
    
    if(type == 2 || type == 5){
        //printf("CURRENTLY PRINTING!\n");
        while (postfix[i] != '\0') {

            if (isspace(postfix[i])) { i++; continue; }

            if (isalnum(postfix[i])) {
                int k = 0;
                while (isalnum(postfix[i])) {
                    token[k++] = postfix[i++];
                }
                token[k] = '\0';
                strcpy(stack[++top], token);
            }

            else if (isOperator(postfix[i])) {
                /*
                    This is for register management for storing intermediate results.
                    temp_reg_id = 3;
                    MAX_TEMP_REG = 8;   >> limit of register usage is 8
                    START_TEMP_REG = 3; >> starting num of usage is 3
                */
                char dest_reg[5];
                sprintf(dest_reg, "R%d", temp_reg_id);          // dest_reg stores the temp_reg_id
                temp_reg_id++;

                if(temp_reg_id > MAX_TEMP_REG){                 // if MAX_TEMP_REG > temp_reg_id, temp_reg_id gets a reset to 3
                    temp_reg_id = START_TEMP_REG;
                }

                /*
                    This is for default source registers
                */
            char src1[5] = "R1";
            char src2[5] = "R2";

                char right[20], left[20];

                strcpy(right, stack[top--]);
                strcpy(left, stack[top--]);

                
                strcpy(result, dest_reg);
                //printf("%s = %s %c %s\n", result, left, postfix[i], right);

                if(left[0] == 'R'){
                    strcpy(src1, left);
                }
                else if (isdigit(left[0])){
                
                    fprintf(initialEdumips, "DADDIU %s, R0, #%s\n", src1, left);
                } else {
                    fprintf(initialEdumips, "LD %s, %s(R0)\n", src1, left);
                }

                if(right[0] == 'R'){
                    strcpy(src2, right);
                }
                else if(isdigit(right[0])){
                    fprintf(initialEdumips, "DADDIU %s, R0, #%s\n", src2, right);
                }else{   
                    fprintf(initialEdumips, "LD %s, %s(R0)\n", src2, right);
                }
                if(postfix[i] == '+'){
                    fprintf(initialEdumips, "DADDU %s, %s, %s\n\n", dest_reg, src1, src2);
                }
                else if(postfix[i] == '-'){
                    fprintf(initialEdumips, "DSUBU %s, %s, %s\n\n", dest_reg, src1, src2);
                }
                else if(postfix[i] == '*'){
                    fprintf(initialEdumips, "DMUL %s, %s, %s\n", dest_reg, src1, src2);
                    fprintf(initialEdumips, "DMULT %s, %s\n", src1, src2);
                    fprintf(initialEdumips, "MFLO %s\n\n", dest_reg);
                }
                else if(postfix[i] == '/'){
                    fprintf(initialEdumips, "DDIV %s, %s, %s\n", dest_reg, src1, src2);
                    fprintf(initialEdumips, "DDIV %s, %s\n", src1, src2);
                    fprintf(initialEdumips, "MFLO %s\n\n", dest_reg);
                }

                strcpy(stack[++top], result);
                i++;
            }
        }
    }
    
    
    if(type == 1){
        if(isNumber(constantExpression) == 1){
            fprintf(initialEdumips, "\nDADDIU R1, R0, #%s\n", constantExpression);
        }else{
        
            fprintf(initialEdumips, "\nLD R1, %s(R0)\n", constantExpression);
        }
    }

    if(type == 4){
        if(isNumber(postfix) == 1){
            fprintf(initialEdumips, "\nDADDIU R1, R0, #%s\n", postfix);
            fprintf(initialEdumips, "SD R1, %s(R0)\n", variable);
        }else{
            fprintf(initialEdumips, "\nLD R1, R0, %s(R0)\n", postfix);
            fprintf(initialEdumips, "SD R1, %s(R0)\n", variable);
        }
    }
    
    if(type == 5 || type == 3){
        
        fprintf(initialEdumips, "SD %s, %s(R0)\n\n", result, variable);
        
    }
    
}

void generate_TAC_from_postfix(char postfix[][MAX], int numPF, char varTemp[][MAX], FILE *initialEduMips, char constantExpression[][MIN], int toggle[], int counter_t, char varVal[][MIN][MIN]) {

      // this condition means the line isn't a declaration
        //printf("\nPRINTING\n");
        int l = 0, k = 0, p = 0, q = 0; // counters for postfix & expression with no operators
        
        for(int i = 0; i < counter_t; i++){
            int temp = toggle[i];
            if(temp == 5){
                // if the statement is from the declaration
                //printf("\nI got here!");
                char *pf = postfix[l++];
                char *var = varTemp[p++];
                char *constExpress = ""; 
                //printf("\n[%d][%s][%d]\n", i, pf, temp);
                intermediate_code(pf, var, initialEduMips, constExpress, temp);
            }
            if(temp == 4){ 
                // if the statement is simple assignment
                //printf("\nI got here!");
                char *pf = varVal[q][1];
                char *var = varVal[q][0];
                q++;
                char *constExpress = ""; 
                //printf("\n[%d][%s][%s][%d]\n", i, pf, var,temp);
                intermediate_code(pf, var, initialEduMips, constExpress, temp);

            }
            if(temp == 3){ //operators
                
                char *pf = postfix[l++];
                char *var = varTemp[p++];
                char *constExpress = ""; 
                //printf("\n[%d][%s][%d]\n", i, pf, temp);
                intermediate_code(pf, var, initialEduMips, constExpress, temp);
            }
            if(temp == 2){ 
                char *pf = postfix[l++];
                char *var = "";
                char *constExpress = ""; 
                //printf("\n[%d][%s][%d]\n", i, pf, temp);
                intermediate_code(pf, var, initialEduMips, constExpress, temp);
            }
            if(temp == 1){
                char *var = "";
                char *pf = "";
                char *constExpress = constantExpression[k++]; 
                //printf("\n[%d][%s][%d]\n", i, constExpress, temp);
                intermediate_code(pf, var, initialEduMips, constExpress, temp);
            }
        }
    //void intermediate_code(char *postfix, char *variable, FILE *initialEdumips, char constantExpression[])
    
}

void file_reset(){
    FILE *initial_mips = fopen("./Final_Output/mips.txt", "w");
    FILE *initial_edumips = fopen("./Initial_Output/initial_edumips.txt", "w");

    FILE *edumipsExecutableText = fopen("./Final_Output/edumipsExecutableText.txt", "w");

    //final translation to binaryrep
    FILE *mipsBinaryCode = fopen("./Final_Output/mipsBinaryCode.txt", "w");
    if(initial_mips == NULL || edumipsExecutableText == NULL || mipsBinaryCode == NULL || initial_edumips == NULL){
        perror("\n*ERROR: COULD NOT LOCATE FILE.*");
        return;
    }
  
}


void generateMips(FILE *final, FILE *mips, FILE *initial){
    char line[MAX];
    //length for ddiv is 13
    //writing first part of the file

    fprintf(final, ".data\n");
    for(int i = 0; i < v; i++){
        fprintf(final, "%s:    .dword\n", symbols[i]);
    }
    fprintf(final, ".code\n");
    
    fseek(initial, 0, SEEK_SET);
   
    while(fgets(line, sizeof(line), initial)){
        line[strcspn(line, "\n")] = '\0';
        if(strlen(line) == 0){
            fprintf(mips, "\n");
            fprintf(final, "\n");
            continue;
        }
        
        if(strlen(line) == 15){
            fprintf(mips, "%s\n", line);
            continue;
        }

        if(strlen(line) == 11){
            fprintf(final, "%s\n", line);
            continue;
        }


        if(strstr(line, "MFLO") != NULL || strstr(line, "DMULT")!=NULL){
            fprintf(final, "%s\n", line);
            continue;
        }

        fprintf(final, "%s\n", line);
        fprintf(mips, "%s\n", line);
    }

}

int isOpCode(char *op){
    if(strcmp(op, "DADDIU") && strcmp(op, "LD") && strcmp(op, "SD")){
        return 1;
    }

    if(strcmp(op, "DADDU") && strcmp(op, "DDIV") && strcmp(op, "DSUBU") &&
       strcmp(op, "DMUL")){
        return 2;
    }
}
char *reg_toBinary(char *result, char *reg) {
    int n = reg[1] - '0';

    for (int i = 5; i >= 0; i--) {
        result[5 - i] = (n & (1u << i)) ? '1' : '0';
    }
    result[6] = '\0';
    return result;
}
int getOffset(char *offset){
    int num = 0;
    for(int i = 0; i < v; i++){
        if(strstr(offset, symbols[i]) != NULL){
            return i*8;
        }
    }
    return -1;
}
char *immediate_toBinary(char *result, char *reg) {
    int n = 0;
    if (isdigit(reg[1])!=0) {
        n = atoi(reg + 1);          // integer value of the register
    }else{
        n = getOffset(reg);         // address of the alias
    }

    int idx = 0;
    
    for (int i = 16; i >= 0; i--) {
        unsigned int mask = 1u << i;                    
        
        result[idx++] = (n & mask) ? '1' : '0';          
        
        /*
           i = 16               --> mask = 65536
           n = 3                --> 0000 0000 0000 0000 0000 0011
           mask = 65536         --> 0000 0001 0000 0000 0000 0000

          result bit = '0'

          i = 1                 --> mask = 2
          n = 3                 --> 0000 0000 0000 0000 0000 0011
          mask = 1              --> 0000 0000 0000 0000 0000 0010   

          result bit = 0000 0000 0000 0000 0000 0010         
        */


        // add spaces in the same order as printing version
        if (i == 12) result[idx++] = ' ';
        if (i == 7)  result[idx++] = ' ';
    }

    result[idx] = '\0';
    return result;
}
void removespace_for_binary(char binary[]){
    int i = 0, k = 0;
    while(binary[i] != '\0'){
        if(!isspace(binary[i])){
            binary[k++] = binary[i];
        }
        i++;
    }
    binary[k] = '\0';
}
void r_instruction(char tokens[][MAX], FILE *binaryrep, char *string){
    char *op = tokens[0];
    
    char rd[7], rs[7], rt[7];
    
    reg_toBinary(rd, tokens[1]); 
    reg_toBinary(rs, tokens[2]); 
    reg_toBinary(rt, tokens[3]);

    char hex_string[MIN];
    char rawbit[MAX];

    memset(rawbit, 0, sizeof(rawbit));
    //DMUL rd,rs,rt
    if(strcmp(op, "DMUL") == 0){
        //printf("000000 %s %s %s 00010 011100", rs, rt, rd);
        // writing to file
        fprintf(binaryrep, "000000 %s %s %s 00010 011100", rs, rt, rd);
        
        strcat(rawbit, "000000");
        strcat(rawbit, rs);
        strcat(rawbit, rt);
        strcat(rawbit, rd);
        strcat(rawbit, "00010011100");
         
    }

    //DDIV
    if(strcmp(op, "DDIV") == 0){
        //printf("000000 %s %s %s 00010 011110", rs, rt, rd);
        // writing to file
        fprintf(binaryrep, "000000 %s %s %s 00010 011110", rs, rt, rd);
      
        strcat(rawbit, "000000");
        strcat(rawbit, rs);
        strcat(rawbit, rt);
        strcat(rawbit, rd);
        strcat(rawbit, "00010011110");
    }
    //DSUBU
    if(strcmp(op, "DSUBU") == 0){
        //printf("000000 %s %s %s 00000 011110", rs, rt, rd);
        // writing to file
        fprintf(binaryrep, "000000 %s %s %s 00000 011110", rs, rt, rd);
        
        strcat(rawbit, "000000");
        strcat(rawbit, rs);
        strcat(rawbit, rt);
        strcat(rawbit, rd);
        strcat(rawbit, "00000011110");
    }

    //DADDU
    if(strcmp(op, "DADDU") == 0){
        //printf("000000 %s %s %s 00000 101101", rs, rt, rd);
        // writing to file
        fprintf(binaryrep, "000000 %s %s %s 00000 101101", rs, rt, rd);
      
        strcat(rawbit, "000000");
        strcat(rawbit, rs);
        strcat(rawbit, rt);
        strcat(rawbit, rd);
        strcat(rawbit, "00000101101");
    }

    unsigned long long decimal_value = strtoull(rawbit, NULL, 2);
    snprintf(hex_string, sizeof(hex_string), "%llX", decimal_value);
    fprintf(binaryrep, "\t;%s\t[hex = %s]\n", string, hex_string);

}


void i_instruction(char tokens[][MAX], FILE *binaryrep, char *string){
    char *op = tokens[0];
    char rt[7], rs[7], immed[MIN], offset[7], base[MIN];
    
    char hex_string[MIN];
    char rawbit[MAX];
    int address = 0;
    memset(rawbit, 0, sizeof(rawbit));  // we fill the entire memory with zero bytes as a reset

    if(strcmp(op, "DADDIU") == 0){
        reg_toBinary(rt, tokens[1]); reg_toBinary(rs, tokens[2]); immediate_toBinary(immed, tokens[3]);

        
        fprintf(binaryrep, "011001 %s %s %s", rs, rt, immed);
        removespace_for_binary(immed);
        
        strcat(rawbit, "011001");        
        strcat(rawbit, rs);
        strcat(rawbit, rt);
        strcat(rawbit, immed);

        
        
        // converting rawbit to its decimal value
        unsigned long long decimal_value = strtoull(rawbit, NULL, 2);

        // %X prints decimal value to hexadecimal string which is stored in hex_string
        snprintf(hex_string, sizeof(hex_string), "%llX", decimal_value);
        fprintf(binaryrep, "\t;%s\t[hex = %s]\n", string, hex_string);

    }

    //LD rt, offset(base)
    // OP | base | rt | offset
    if(strcmp(op, "LD") == 0){
        // converting registers and immediate to binary numbers

        reg_toBinary(rt, tokens[1]); //Register
        immediate_toBinary(offset, tokens[2]); // variable
        reg_toBinary(base, tokens[3]); // register
        
        
        fprintf(binaryrep, "110111 %s %s %s", base, rt, offset);

        //offset contains three spaces to improve readability
        removespace_for_binary(offset);

        // all of these gets concatenated to efficiently convert it to its hexadecimal string
        strcat(rawbit, "110111");
        strcat(rawbit, base);
        strcat(rawbit, rt);
        strcat(rawbit, offset);

        int ad = getOffset(tokens[2]);
        
        // converting rawbit to its decimal value
        unsigned long long decimal_value = strtoull(rawbit, NULL, 2);

        // %X prints decimal value to hexadecimal string which is stored in hex_string
        snprintf(hex_string, sizeof(hex_string), "%llX", decimal_value);
        fprintf(binaryrep, "\t;%s\t[hex = %s] [Address = 0x000%d]\n", string, hex_string, ad);
    }

    //SD rt, offset(base)
    // OP | base | rt | offset
    if(strcmp(op, "SD") == 0){
        reg_toBinary(rt, tokens[1]); 
        immediate_toBinary(offset, tokens[2]); //letter
        reg_toBinary(base, tokens[3]); // R0

        
        fprintf(binaryrep, "111111 %s %s %s", base, rt, offset);
        removespace_for_binary(offset);
        strcat(rawbit, "111111");
        strcat(rawbit, base);
        strcat(rawbit, rt);
        strcat(rawbit, offset);
        address = getOffset(tokens[2]); 
        
        // efficiently convers rawbit to its decimal value
        unsigned long long decimal_value = strtoull(rawbit, NULL, 2);

        // %X prints the hexadecimal of the decimal value then it becomes a the resulted formatted string that is stored in hex_string
        snprintf(hex_string, sizeof(hex_string), "%llX", decimal_value);
        fprintf(binaryrep, "\t;%s\t[hex = %s] [Address = 0x0000%d]\n", string, hex_string, address);
    }

    
}


void generate_machinelanguage(FILE *mips, FILE *binary){
    char tokens[MAX][MAX];
    char line[MAX];

    fseek(mips, 0, SEEK_SET);                           //moves the file pointer to the beginning of the file to avoid misbehaviors
    while(fgets(line, sizeof(line), mips)){
        
        line[strcspn(line, "\n")] = '\0';

        if(strlen(line) == 0){
            continue;
        }
        
        int tokenCount = tokenizer(2, tokens, line);
        int type = isOpCode(tokens[0]);                 // returns what type of instruction based on the first token

        if(type == 1){
            r_instruction(tokens, binary, line);
        }
        if(type == 2){
            i_instruction(tokens, binary, line);
        }
    }
}


int main(){

    file_reset();

    FILE *code = fopen("./arith_input_files/sample.txt", "r");
    //initial assembly code, not clean
    FILE *initial_mips = fopen("./Final_Output/mips.txt", "a+");
    FILE *initial_edumips = fopen("./Initial_Output/initial_edumips.txt", "a+");

    //final assembly code, clean
    FILE *edumipsExecutableText = fopen("./Final_Output/edumipsExecutableText.txt", "a+");

    //final translation to binaryrep
    FILE *mipsBinaryCode = fopen("./Final_Output/mipsBinaryCode.txt", "a+");

    if(code == NULL || initial_mips == NULL || edumipsExecutableText == NULL || mipsBinaryCode == NULL || initial_edumips == NULL){
        perror("\n*ERROR: COULD NOT LOCATE FILE.*");
        return 0;
    }

    char string[MAX];
    char tokens[MAX][MAX];
    char postfix[MAX][MAX] = {'\0'};
    char varVal[MAX][MIN][MIN];
    char varTemp[MAX][MAX];
    int check = 0, numPF = 0, error = 0;

    //it validates the textfile line by line
    while(fgets(string, sizeof(string), code)){
        string[strcspn(string, "\n")] = '\0';
        if(strlen(string) == 0){
             continue;                                   // skips empty line
        }

        printf("Line: %s", string);

        char expression[MIN][MAX];
        int count = 0;
        int k = 0, l = 0;

        if(strstr(string, "int") != NULL){
            for(int i = k; i < strlen(string); i++){
                char temp[MAX];
                if(string[i] != ';' && (string[i+1] != ' ' || string[i+1] != '\0')){
                    temp[count++] = string[i];
                    //printf("[%s]\n", temp);
                }else if(string[i] == ';'){
                    k = i++;
                    temp[count++] = ';';
                    temp[count] = '\0';
                    sprintf(expression[l], "%s", temp);
                    l++;
                    count = 0;
                    temp[0] = '\0';
                }else{
                    printf("*1ERROR: undeclared identifier*\n");
                    break;
                }
            }
        }else{
            k = 0;
            for(int i = k; i < strlen(string); i++){
                char temp[MAX];
                if(string[i] != ';' && (string[i+1] != ' ' || string[i+1] != '\0')){
                    temp[count++] = string[i];
                    //printf("[%s]\n", temp);
                }else if(string[i] == ';'){
                    k = i++;
                    temp[count++] = ';';
                    temp[count] = '\0';
                    sprintf(expression[l], "%s", temp);
                    l++;
                    count = 0;
                    temp[0] = '\0';
                }else{
                    printf("*2ERROR: undeclared identifier*\n");
                    break;
                }
            }
        }
        /*
             tokenizer   >> before the function tokenizes the string it initially checkings the following:
                             >>  [data type][expression][,][expression][;]
                             >>  [variable][=][mathematical expression][;]
                             >>  [variable][=][value][;]
                             >>  it initially checks the outer structure
                                 >>  checks repetitive use of "int"
                                 >>  checks if uses char or float declaration (these are not accepted in the code)
                                 >>  it returns 0, if it doesn't pass through the initial checking
                                 >>  otherwise, it proceeds on tokenizing the rest of the string
                                 >>  it return the number of tokens if successful
        */
        
        for(int i = 0; i < l; i++){
            
            char *str = expression[i];
            //printf("\nEXPRESSION [%s]\n", str);
            int comCheck = 0;
            compoundChecker(str, &comCheck);
                if(comCheck != 0){
                    strcpy(expression[i], str);
                }
            int unaCheck = 0;
            unaryChecker(str, &unaCheck);
                if(unaCheck != 0){
                    return 0;
                }
            //insert unary handler here
            int tokCount = tokenizer(1, tokens, str);

                
                
                if(tokCount == 0){                        // data type is incorrect
                    fseek(initial_edumips, 0, SEEK_SET);
                    file_reset();
                    //return 0;
                }
            
            /*  ARGUMENTS in syntax_analyzer
                varVal  >>  a 3D string array that stores both variables and values
                        >>  will be used in mips, when we define and save them
                tokens  >>  [int][expression][,][expression][,][expression][;]
                        >>  variables and value/mathematical expression in expression gets extracted and examined
                        >>  it's made this way for my comfort in easily tracking and debugging
                postfix >>  stores the order of precedence of the mathematical expression that will be evaluated
                        >>  i.e., "a+b+c" --> ab + c +
                        >>  i.e., ""
                numPF   >>  this is the number of times that there declared variables with mathematical expression as its value
                        >>  int a = 1 + 2, b, c, d = b * c;
                        >>  there are 2 variables with assigned mathematical expressions
                        >>  the postfix (2D string array) stores these postfix
                            >> each postfix gets evaluated through a loop
                            >> numPF is used as a counter for this loop
                varTemp >>  this will be used during generation of TAC and initial writing of mips code
                        >>  int a = a + b + c;
                        >>  varTemp[0] = "a"
                        >>  SD R1, varTemp(R0)
                        >>  the only thing that is initially written in mips are mathematical expressions
            */
            /*  
                syntax_analyzer >>  it checks for variables that are repetitively declared
                                >>  it validates variable names
                                    >> during validation, this is where we also store simple assignments
                                    >> "a = 10" "b = 20" --> these get stored in varVal
                                >>  mathematical expressions gets extracted and validated
                                    >> [1] it goes through contextualChecker(temp, declared, variable)
                                        >> temp     -> extracted mathematical expression from token [expression]
                                        >> declared -> global variable counter for the number of declared variables
                                        >> variable -> extracted variable from token [expression]
                                        >>> checks if temp uses variables that are undeclared
                                    >> [2] after passing [1], enters parenthesisChecker(infix, postfix, numPF)
                                        >> infix    -> stores temp refer to [1]
                                        >> postfix  -> 2D string array that stores the order of precedence of the mathematical expression
                                                    -> we get the postfix from this function through stacking
                                        >> numPF    -> index of postfix
                                                    -> postfix[numPF]
                                                    -> numPF gets incremented if the string has another variable with an assigned math
                                        >>> parenthesis checker checks infix if it has a balanced or imbalanced parenthesis
                                    >> [3] postfix enters mathematical_expression(postfix, &error, numPF)
                                                    ->  2 operands gets pushed to the stack
                                                    ->  it only gets popped when it encounter an operator next
                                                    ->  it checks if there are two operands first before it gets popped
                                                    ->  return 1 and no change in value of error

            */
            char constantExpression[MIN][MIN];        // this is for constant expressions like "10;", "a;"
            int toggle[MIN];
            int counter_t = 0;
            int varValCnt = 0;
            numPF = syntax_analyzer(tokens, tokCount, postfix, &error, varTemp, varVal, constantExpression, toggle, &counter_t, &varValCnt);
                
                if(numPF == 0 && error == 1){
                    fseek(initial_edumips, 0, SEEK_SET);
                    file_reset();
                    return 0;
                }else{
                    printf("\n");
                    if(strstr(str, "int") != NULL){
                        printf("\t=== Correct Declaration\n");
                    }
                    if(strstr(str, "=") != NULL){
                        printf("\t=== Correct Assignment\n");
                    }
                    if(strstr(str, ";") != NULL){
                        printf("\t=== Correct Syntax\n");
                    }
                }  
            /*
                generate_TAC_from_postfix   >>  this generates a TAC of the mathematical expressions
                                            >>  this is only exclusive for mathematical expressions
                                            >>  initially writes mips on file initial_edumips
            */
            generate_TAC_from_postfix(postfix, numPF, varTemp, initial_edumips, constantExpression, toggle, counter_t, varVal);
        }
       
    }

    printf("\n-------Run Successfully-------\n");

    /*
        generateMIPS    >>  creates
    */
    generateMips(edumipsExecutableText, initial_mips, initial_edumips);

    generate_machinelanguage(initial_mips, mipsBinaryCode);

    fclose(edumipsExecutableText);
    fclose(initial_edumips);
    fclose(initial_mips);
    fclose(mipsBinaryCode);
    fclose(code);
    return 0;
}