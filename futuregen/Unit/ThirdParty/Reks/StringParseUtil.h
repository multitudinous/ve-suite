/**
 *  StringParseUtil.h
 *
 */

// Copyright 2002  Reaction Engineering International
// by Yang

#ifndef CKINTERP_STR_PARSE_UTIL_H
#define CKINTERP_STR_PARSE_UTIL_H
#pragma warning(disable : 4786)

#include <vector>
#include <string>
#include <iostream>
#include <map>
#include "GlobalConst.h"


#define BUFFER_MAX 5000 

using namespace std;

//This is the structure for a atomic formula of a specie
//the symbol is the element name
typedef struct atomf
{
	char symbol[2];
	int num;
} AtomicFormula;

//Ignore Comments get delete anything started with !
int ignore_comments(char* current_line);

//if the the line started with !@
//this is a REI format thermo line
//return -123 as the flag, otherwise just list ignore_comments
int ignore_REIcomments(char* current_line);

//Get rid of the space after 'E' or 'e' of a scientific number
//some old fortran file has a number like 1.2E 06, which is not recognizable by C
//this function transfer it to 1.2E06
int ignore_Espace(char* current_line);

//ingore any character occured in the second argument
void ignore_char(char* current_line, char* ign_chs);

//get tokens delemitted by one or more space, 
//for slashed value like /   1.2,   1.3  /
//the returned token will be /1.2,1.3/, tokens vector is cleared first in the function
int get_token(char* current_line, vector<string>& toks);

//get tokens delemitted by one or more space, 
//for slashed value like /   1.2,   1.3  /
//the returned token will be /1.2,1.3/, tokens vector is not cleared
int get_token2(char* current_line, vector<string>& toks);

//get tokens delemitted by one or more space, 
//the returned token will be /1.2,1.3/, tokens vector is not cleared
int get_token3(char* current_line, vector<string>& toks);

//this get tokens delemitted by one or more space
//doesn't care about / so it can deal with directory name
int get_token4(char* current_line, vector<string>& toks);

//decide if this token a number
//just by checking if the first character is a digit, or '.' or sign '-'  or not
bool is_number(string token);

//check is string s1 match string s2
//if the s2 is s1's prefix, it return true
//match is case insensitive
bool match(const string& s1, const string& s2) ;

//check if word is a keyword for the mechnism file
//keywords	return value
//ELEM		1 
//SPEC		2
//THERM		3
//REAC		4
//END		5
//if not a keyword, return 0. And the check is case insensitive
int is_keyword(string word); 

//Unit key word
//

int unit_keyword(string word);

//if the first character of the token match any of  the character's in the second arg
//return false, otherwise return true
bool check_initial(string token, char* bad);

//treat the buffer as one line
//first ignore all head space
//then get a string token less than 16 characters long (by hit anther space)
//if a name is grabed, return true, otherwise return false
bool grab_spec_name(char* buffer, int &pos, string& spec_name);

//advance to next non-space position of the line
int next_nonspace(char *buffer,  int pos);

//treat buffer as 1 line, if the end of the line is a '&'
//return true, which means next line is the continue part of this line
//else return false
bool continued(char* buffer);

// if the character string contain a sub string of "END" or "end"
bool check_END(char* buffer);

//check if there is more ')' than '(' in the spec
//this is because of the special action taken by the line parser
//CH(S) just mean on specie
//(CH) means this is pressure dependent reaction
bool check_parenthis(char* spec);

//delete all ' ' '\t' inside the character array
void eatspace(char *result);



#endif


