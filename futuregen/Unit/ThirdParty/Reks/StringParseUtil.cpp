/**
 *  StringParseUtil.cpp
 *
 */

// Copyright 2002  Reaction Engineering International
// by Yang
/////////////////////////////////////////////////////

#include "StringParseUtil.h"

//Ignore Comments get delete anything started with !
int ignore_comments(char* current_line)
{
	int i=0;
	while ((current_line[i]!='\0')&&(current_line[i]!='\n'))
		if (current_line[i]!='!')
			i++;
		else
			current_line[i] = '\0';
	return i;
}

//if the the line started with !@
//this is a REI format thermo line
//return -123 as the flag, otherwise just list ignore_comments
int ignore_REIcomments(char* current_line)
{
	int i=0;
	char temp[BUFFER_MAX];
	int j=0;
	int flag = 0;
	while ((current_line[i]!='\0')&&(current_line[i]!='\n'))
		if (current_line[i]!='!')
		{
			temp[j++]=current_line[i++];
		}
		else if (current_line[++i]=='@') //'@' is the rei sign for free style lines
		{
			i++;
			flag = -123;
		}
		else
		{
			temp[j]='\0';
			break;
		}
		temp[j]='\0';
	strcpy(current_line,temp);
	return flag;
}

//Get rid of the space after 'E' or 'e' of a scientific number
//some old fortran file has a number like 1.2E 06, which is not recognizable by C
//this function transfer it to 1.2E06
int ignore_Espace(char* current_line)
{
	char *temp;
	int len = strlen(current_line);
	temp = new char[len+1];
	int j=0;
	bool neglect=false;
	int i;

	for (i=0; i<len; i++)
	{
		if ((current_line[i]=='E')||(current_line[i]=='e'))
		{
			neglect=true;
			temp[i]=current_line[i];
			continue;
		}
		if (neglect==true)
		{
			if (current_line[i]==' ')
			{   if ((current_line[i+1]>='0')&&(current_line[i+1]<='9'))
					temp[i]='+';
				else
					temp[i]=current_line[i];
			}
			else
				temp[i]=current_line[i];
			neglect=false;
			continue;
		}
		temp[i]=current_line[i];
	}
	temp[i]='\0';
	strcpy(current_line, temp);
	delete temp;
	return 0;
}

//ingore any character occured in the second argument
void ignore_char(char* current_line, char* ign_chs)
{
  
  char *result;
  result = new char[strlen(current_line)];
  int k=0;

  for (int i=0; i<strlen(current_line); i++)
    {
      for (int j=0; j<strlen(ign_chs); j++)
		if (current_line[i]==ign_chs[j])
			i++;
      result[k++]=current_line[i];
    }
  result[k]='\0';
  strcpy(current_line, result);
  delete result;

}

//get tokens delemitted by one or more space, 
//for slashed value like /   1.2,   1.3  /
//the returned token will be /1.2,1.3/, tokens vector is cleared first in the function
int get_token(char* current_line, vector<string>& toks)
{
	char* token;
	int i=0;
	token = strtok(current_line, "/ ,\t\n");

	toks.clear();
	while( token )
	{
		i++;
		toks.push_back(string(token));
		token = strtok(NULL, "/ ,\t\n");
	}

  return i;
}

//get tokens delemitted by one or more space, 
//for slashed value like /   1.2,   1.3  /
//the returned token will be /1.2,1.3/, tokens vector is not cleared
int get_token2(char* current_line, vector<string>& toks)
{
	char* token;
	int i=0;
	token = strtok(current_line, "/ ,\t\n");
	while( token )
	{
		i++;
		toks.push_back(token);
		token = strtok(NULL, "/ ,\t\n");
	}

  return i;
}

int get_token3(char* current_line, vector<string>& toks)
{
	char* token;
	int i=0;
	token = strtok(current_line, " \t\n");
	while( token )
	{
		i++;
		toks.push_back(token);
		token = strtok(NULL, " \t\n");
	}

  return i;
}

//this get tokens delemitted by one or more space
//doesn't care about / so it can deal with directory name
int get_token4(char* current_line, vector<string>& toks)
{
	char* token;
	int i=0;
	token = strtok(current_line, " \t\n");

	toks.clear();
	while( token )
	{
		i++;
		toks.push_back(token);
		token = strtok(NULL, " \t\n");
	}

  return i;
}

//decide if this token a number
//just by checking if the first character is a digit or '.' or sign '-' or not
bool is_number(string token)
{
	if ((((token.c_str())[0]>='0') &&((token.c_str())[0]<='9'))
		||((token.c_str())[0]=='.')
		||(((token.c_str())[0]=='-')
			&&(((token.c_str())[1]>='0') &&((token.c_str())[1]<='9')))
		||(((token.c_str())[0]=='-')
			&&((token.c_str())[1]=='.'))
			)
		return true; //this is a number
	else
		return false;
}

//check is string s1 match string s2
//if the s2 is s1's prefix, it return true
//match is case insensitive
bool match(const string& s1, const string& s2)
{
    size_t n = s2.size();
    if (s1.size() < n) return false;
    for (size_t i = 0; i < n; i++) 
	if (s2[i] != '*' && (toupper(s1[i]) != toupper(s2[i]))) return false;
    return true;
}

//check if word is a keyword for the mechnism file
//keywords	return value
//ELEM		1 
//SPEC		2
//THERM		3
//REAC		4
//END		5
//if not a keyword, return 0. And the check is case insensitive
int is_keyword(string oword) 
{
	char temp[256];
	int i;
	string word;

	strcpy(temp,oword.c_str());
	for (i=0; i<strlen(temp); i++)
		temp[i]=(char)toupper(temp[i]);
	temp[i]='\0';
	word = string(temp);

    if (match(word, "ELEM"))
      return 1; //element section
    else if (match(word, "SPEC"))
      return 2; //spec section
    else if (match(word, "THERM"))
      return 3; //thermo data section
    else if (match(word, "REAC"))
      return 4; //reaction section
    else if (match(word, "END"))
      return 5; //symbol for ending a section
    else 
      return 0; //not a key word
}

int unit_keyword(string oword)
{
	char temp[256];
	int i;
	string word;

	strcpy(temp,oword.c_str());
	for (i=0; i<strlen(temp); i++)
		temp[i]=(char)toupper(temp[i]);
	temp[i]='\0';
	word = string(temp);

    if (match(word, "CAL/MOLE"))
      return 1; //element section
    else if (match(word, "KCAL/MOLE"))
      return 2; //spec section
    else if (match(word, "JOULE/MOLE"))
      return 3; //thermo data section
    else 
      return 0; //not a key word

}

//if the first character of the token match any of  the character's in the second arg
//return false, otherwise return true
bool check_initial(string token, char* bad)
{
  char initial = token[0];
  
  for (int i=0; i<strlen(bad); i++)
    if (initial == bad[i])
      return false;
  return true;
}

//treat the buffer as one line
//first ignore all head space
//then get a string token less than 16 characters long (by hit anther space)
//if a name is grabed, return true, otherwise return false
bool grab_spec_name(char* buffer, int &pos, string& spec_name)
{
	int i, j;
	bool head_space = true;
	int len = strlen(buffer);
	
	char temp[17];
	
	j=0;
	

	for (i=pos; i<len; i++)
		if ((buffer[i]!=' ')&&(buffer[i]!='\n')&&(buffer[i]!='\t'))
		{
			temp[j++]=buffer[i];
			if (head_space)
				head_space = false;
			if (j>16) //no specie name can exceed 16 characters // cut here
			{
				temp[j]='\0';
				while ((buffer[i]==' ')||(buffer[i]=='\n')||(buffer[i]=='\t'))
					i++; //advance to next non-space position;
				pos = i-1;
				spec_name = temp;
				return true;
			}

		}
		else
			if (head_space)
				continue; //screw all head space
			else //cut here
			{
				temp[j]='\0';
				while ((buffer[i]==' ')||(buffer[i]=='\n')||(buffer[i]=='\t'))
					i++; //advance to next non-space position;
				pos = i-1;
				spec_name = temp;
				return true;
			};
	if (j>0) //something there, cut it
	{
		pos = i-1;
		temp[j]='\0';
		spec_name = temp;
		return true;
	}
	return false;
				
}

//advance to next non-space position of the line
int next_nonspace(char *buffer,  int pos)
{
	int i;
	i=pos;
	while (((buffer[i]==' ')||(buffer[i]=='\t'))&&(buffer[i]!='\0'))
		i++; //advance to next non-space position;
	return i;
		
}

//treat buffer as 1 line, if the end of the line is a '&'
//return true, which means next line is the continue part of this line
//else return false
bool continued(char* buffer)
{
	int len = strlen(buffer);
	if (buffer[len-1]=='&')
		return true;
	else
		return false;
}

// if the character string contain a sub string of "END" or "end"
bool check_END(char* buffer)
{
	if (strstr(buffer,"END")||strstr(buffer,"end"))
		return true;
	else
		return false;
}

//check if there is more ')' than '(' in the spec
//this is because of the special action taken by the line parser
//CH(S) just mean on specie
//(CH) means this is pressure dependent reaction
bool check_parenthis(char* spec)
{
	int i;
	int j=0;
	int last_pos;
	for(i=0; i<strlen(spec); i++)
	{
		if (spec[i]=='(')
			j++;
		if (spec[i]==')')
		{
			last_pos=i;
			j--;
		}
	}


	if (j<0)
	{
		spec[last_pos]='\0';
		return true;
	}
	else
		return false;
}

//delete all ' ' '\t' inside the character array
void eatspace(char *result)
{
	char temp[BUFFER_MAX];
	int j=0;
	for (int i=0; i<strlen(result); i++)
		if (result[i]!=' '||result[i]!='\t')
			temp[j++]=result[i];
	temp[j]='\0';
	strcpy(result,temp);
	return;
}

