#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

using namespace std;

const int BUFFER_MAX = 1024;
string MOD_name;
vector<string> Double_name;
vector<string> String_name;
vector<string> Int_name;
vector<string> Double1D_name;
vector<string> String1D_name;
vector<string> Int1D_name;

void parse_inp(const char* defname);
int is_inp_keyw(string oword);
int ignore_comments(char* current_line);
int get_token(char* current_line, vector<string>& toks);
bool match(const string& s1, const string& s2);
void GenCode();

int main(int argc, char* argv[]) //This is a Cheezy wizard. take a mod.def as input
{
  if (argc<2)
    printf("The Format is ModWiz [module definition file]\n");
  else
    {
      parse_inp(argv[1]);
      GenCode();
    }
}

void parse_inp(const char * defname)
{  
  ifstream inp(defname);
  int line_number=0;
  bool end_of_file = false;
  char buffer[BUFFER_MAX+1];
  vector<string> toks;
  
  
  Double_name.clear();
  String_name.clear();
  Int_name.clear();
  Double1D_name.clear();
  String1D_name.clear();
  Int1D_name.clear();

   do 
    {
      end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
      line_number++;
      ignore_comments(buffer);

      if (get_token(buffer, toks)>0)
	switch(is_inp_keyw(toks[0]))
	{
	case 1:
	  MOD_name = toks[1];
	  break;
	case 2:
	  Double_name.push_back(toks[1]);
	  break;
	case 3:
	  String_name.push_back(toks[1]);
	  break;
	case 4:
	  Int_name.push_back(toks[1]);
	  break;
	case 5:
	  Double1D_name.push_back(toks[1]);
	  break;
	case 6:
	  String1D_name.push_back(toks[1]);
	  break;
	case 7:
	  Int1D_name.push_back(toks[1]);
	  break;
	case 8:
	  inp.close();
	  return;
	default:
	  inp.close();
	  return;
	} 
      else
	;
    } while(!end_of_file);
	  
}

int is_inp_keyw(string oword)
{ 
  char temp[256];
  int i;
  string word;
  
  //strcpy(temp,oword.c_str());
  //  for (i=0; i<strlen(temp); i++)
  //  temp[i]=(char)toupper(temp[i]);
  //temp[i]='\0';
  word = oword;//string(temp);
  
  if (match(word, "MOD_NAME"))
    return 1; //gas temperature
  else if (match(word, "Double"))
    return 2; //gas pressure
  else if (match(word, "String"))
    return 3; //reaction time
  else if (match(word, "Int"))
    return 4; //output time slice
  else if (match(word, "Double1D"))
    return 5; //reactants
  else if (match(word, "String1D"))
    return 6; //symbol for ending
  else if (match(word, "Int1D"))
    return 7;
  else if (match(word, "END"))
    return 8;
  else
    return 0; //not a key word
}

//Ignore Comments get delete anything started with !
int ignore_comments(char* current_line)
{
  int i=0;
  while ((current_line[i]!='\0')&&(current_line[i]!='\n'))
    if (current_line[i]!='#')
      i++;
    else
      current_line[i] = '\0';
  return i;
}

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

//check is string s1 match string s2
//if the s2 is s1's prefix, it return true
//match is case insensitive
bool match(const string& s1, const string& s2)
{
    size_t n = s2.size();
    if (s1.size() != n) 
      {
	//printf("%s,  %s Not same size\n", s1.c_str(), s2.c_str());
	return false;
      }
    for (size_t i = 0; i < n; i++) 
	if (s2[i] != '*' && s1[i] != s2[i]) 
	  {
	    //printf("%d: %c vs %c\n", i, s1[i], s2[i]);
	    return false;
	  }
    return true;
}

//==========================================
void GenCode()
{
  string mod_h;
  string mod_cpp;
  string mod_ui_h;
  string mod_ui_cpp;
  char buffer[BUFFER_MAX+1];
  char buffer2[BUFFER_MAX+1];
  char out_buf[BUFFER_MAX+1];

  mod_h = MOD_name+".h";
  mod_cpp = MOD_name+".cpp";
  mod_ui_h = MOD_name+"_UI.h";
  mod_ui_cpp = MOD_name+"_UI.cpp";

  printf("Make new directory %s\n", MOD_name.c_str());

  mkdir(MOD_name.c_str(), S_IRWXU|S_IRWXG);
  chdir(MOD_name.c_str());
  //no more file copies
  //system("cp ../Makefile.incl ./");
  //system("cp ../interface.h ./");
  //system("cp ../package.h ./");
  //system("cp ../string_ops.h ./");
  //system("cp ../Plugin_base.h ./");
  //system("cp ../UIDialog.h ./");
  ifstream fh_inp("../TEMPLATE.h");
  ifstream fcpp_inp("../TEMPLATE.cpp");
  ifstream fuih_inp("../TEMPLATE_UI.h");
  ifstream fuicpp_inp("../TEMPLATE_UI.cpp");
  ifstream fmake_inp("../Makefile.TEMPLATE");
  FILE *f_h = fopen(mod_h.c_str(), "w");
  FILE *f_cpp = fopen(mod_cpp.c_str(), "w");
  FILE *f_ui_h = fopen(mod_ui_h.c_str(), "w");
  FILE *f_ui_cpp = fopen(mod_ui_cpp.c_str(), "w");
  FILE *f_make = fopen("Makefile", "w");
  char* pos_temp;
  char* if_public;
  char* if_constructor;
  char* if_UIDialog;
  bool already_one_public = false;
  bool end_of_file;
  int len;
  int i;

  printf("Generating Makefile\n");
  do {
    end_of_file = (fmake_inp.getline(buffer, BUFFER_MAX, '\n')).eof();
    pos_temp = strstr(buffer, "TEMPLATE");
    strcpy(buffer2, buffer);
    if (pos_temp)
      {
	*pos_temp='\0';
	strcpy(out_buf, buffer);
	len = strlen(out_buf);
	strcpy(&out_buf[len], MOD_name.c_str());
	len = strlen(out_buf);
	strcpy(&out_buf[len], &pos_temp[8]);
      }
    else
      strcpy(out_buf, buffer);
    fprintf(f_make, "%s\n", out_buf);

  }while(!end_of_file);
  fclose(f_make);
  fmake_inp.close();

  printf("Generating %s\n", mod_h.c_str());
  do {
    end_of_file = (fh_inp.getline(buffer, BUFFER_MAX, '\n')).eof();
    pos_temp = strstr(buffer, "TEMPLATE");
    strcpy(buffer2, buffer);
    if (pos_temp)
      {
	*pos_temp='\0';
	strcpy(out_buf, buffer);
	len = strlen(out_buf);
	strcpy(&out_buf[len], MOD_name.c_str());
	len = strlen(out_buf);
	strcpy(&out_buf[len], &pos_temp[8]);
      }
    else
      strcpy(out_buf, buffer);
    fprintf(f_h, "%s\n", out_buf);
    if_public = strstr(buffer2, "public:");
    if (if_public)
      if (already_one_public)
      {
	//Let print out the member variables;
	for (i=0; i<Double_name.size(); i++)
	  fprintf(f_h, "  double %s;\n", Double_name[i].c_str());
	for (i=0; i<String_name.size(); i++)
	  fprintf(f_h, "  string %s;\n", String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_h, "  long %s;\n", Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	  fprintf(f_h, "  vector<double> %s;\n", Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_h, "  vector<string> %s;\n", String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_h, "  vector<long> %s;\n", Int1D_name[i].c_str());
	
      }
      else
	already_one_public=true;
  }while(!end_of_file);
  fclose(f_h);
  fh_inp.close();

  printf("Generating %s\n", mod_cpp.c_str());
  do {
    end_of_file = (fcpp_inp.getline(buffer, BUFFER_MAX, '\n')).eof();
    pos_temp = strstr(buffer, "TEMPLATE");
    strcpy(buffer2, buffer);
    if (pos_temp)
      {
	*pos_temp='\0';
	strcpy(out_buf, buffer);
	len = strlen(out_buf);
	strcpy(&out_buf[len], MOD_name.c_str());
	len = strlen(out_buf);
	strcpy(&out_buf[len], &pos_temp[8]);
      }
    else
      strcpy(out_buf, buffer);
    fprintf(f_cpp, "%s\n", out_buf);
    //if ( strstr(buffer2, ":TEMPLATE"))
    //printf("%s", buffer);
    if_constructor = strstr(buffer2, ":TEMPLATE");
    if (if_constructor)
      {
	fprintf(f_cpp, "{\n");
	//Let print out the Register of member variables;
	for (i=0; i<Double_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Double_name[i].c_str(), Double_name[i].c_str());
	for (i=0; i<String_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", String_name[i].c_str(), String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Int_name[i].c_str(), Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Double1D_name[i].c_str(), Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", String1D_name[i].c_str(), String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Int1D_name[i].c_str(), Int1D_name[i].c_str());
	fprintf(f_cpp, "}\n");
      }

    if_UIDialog = strstr(buffer2, "TEMPLATE_UI_Dialog");
    if (if_UIDialog)
      {
	fseek(f_cpp, -2, SEEK_CUR);
	fprintf(f_cpp, "(parent, -1,\n");
	//Let print out the Register of member variables;
	for (i=0; i<Double_name.size(); i++)
	  fprintf(f_cpp, "     &%s,\n", Double_name[i].c_str());
	for (i=0; i<String_name.size(); i++)
	  fprintf(f_cpp, "     &%s,\n", String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_cpp, "     &%s,\n", Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	  fprintf(f_cpp, "     &%s,\n", Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_cpp, "     &%s,\n", String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_cpp, "     &%s,\n", Int1D_name[i].c_str(), Int1D_name[i].c_str());
	fseek(f_cpp, -2, SEEK_CUR); //wind back the last , and \n
	fprintf(f_cpp, ");\n"); 
      }
  }while(!end_of_file);
  fclose(f_cpp);
  fcpp_inp.close();
  
  already_one_public = false;

  printf("Generating %s\n", mod_ui_h.c_str());
  do {
    end_of_file = (fuih_inp.getline(buffer, BUFFER_MAX, '\n')).eof();
    pos_temp = strstr(buffer, "TEMPLATE");
    strcpy(buffer2, buffer);
    if (pos_temp)
      {
	*pos_temp='\0';
	strcpy(out_buf, buffer);
	len = strlen(out_buf);
	strcpy(&out_buf[len], MOD_name.c_str());
	len = strlen(out_buf);
	strcpy(&out_buf[len], &pos_temp[8]);
      }
    else
      strcpy(out_buf, buffer);
    fprintf(f_ui_h, "%s\n", out_buf);
    if_public = strstr(buffer2, "public:");
    if (if_public)
      if (already_one_public)
      {
	//Let print out the member variables;
	for (i=0; i<Double_name.size(); i++)
	  fprintf(f_ui_h, "  double* p_%s;\n", Double_name[i].c_str());
	for (i=0; i<String_name.size(); i++)
	  fprintf(f_ui_h, "  string* p_%s;\n", String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_ui_h, "  long* p_%s;\n", Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	  fprintf(f_ui_h, "  vector<double>* p_%s;\n", Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_ui_h, "  vector<string>* p_%s;\n", String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_ui_h, "  vector<long>* p_%s;\n", Int1D_name[i].c_str());
	
      }
      else
	{
	  //First pulbic, should print the constructor with the GUI variables
	  fprintf(f_ui_h, "  %s_UI_Dialog(wxWindow* parent, int id,\n", MOD_name.c_str());
   
	  for (i=0; i<Double_name.size(); i++)
	    fprintf(f_ui_h, "          double* %s,\n", Double_name[i].c_str());
	  for (i=0; i<String_name.size(); i++)
	    fprintf(f_ui_h, "          string* %s,\n", String_name[i].c_str());
	  for (i=0; i<Int_name.size(); i++)
	    fprintf(f_ui_h, "          long* %s,\n", Int_name[i].c_str());
	  for (i=0; i<Double1D_name.size(); i++)
	    fprintf(f_ui_h, "          vector<double>* %s,\n", Double1D_name[i].c_str());
	  for (i=0; i<String1D_name.size(); i++)
	    fprintf(f_ui_h, "          vector<string>* %s,\n", String1D_name[i].c_str());
	  for (i=0; i<Int1D_name.size(); i++)
	    fprintf(f_ui_h, "          vector<long>* %s,\n", Int1D_name[i].c_str());
	  fseek(f_ui_h, -2, SEEK_CUR); //wind back the last , and \n
	  fprintf(f_ui_h, ");\n"); 
	  already_one_public=true;
	}

  }while(!end_of_file);
  fclose(f_ui_h);
  fuih_inp.close();

  printf("Generating %s\n", mod_ui_cpp.c_str());
  do {
    end_of_file = (fuicpp_inp.getline(buffer, BUFFER_MAX, '\n')).eof();
    pos_temp = strstr(buffer, "TEMPLATE");
    strcpy(buffer2, buffer);
    if (pos_temp)
      {
	*pos_temp='\0';
	strcpy(out_buf, buffer);
	len = strlen(out_buf);
	strcpy(&out_buf[len], MOD_name.c_str());
	len = strlen(out_buf);
	strcpy(&out_buf[len], &pos_temp[8]);
      }
    else
      strcpy(out_buf, buffer);
    fprintf(f_ui_cpp, "%s\n", out_buf);
    if_constructor = strstr(buffer2, ":TEMPLATE");
    if (if_constructor)
      {
	//Let print out the parameter constructor;
	fprintf(f_ui_cpp, "(wxWindow* parent, int id,\n", MOD_name.c_str());
	
	for (i=0; i<Double_name.size(); i++)
	  fprintf(f_ui_cpp, "  double* %s,\n", Double_name[i].c_str());
	for (i=0; i<String_name.size(); i++)
	  fprintf(f_ui_cpp, "  string* %s,\n", String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_ui_cpp, "  long* %s,\n", Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	    fprintf(f_ui_cpp, "  vector<double>* %s,\n", Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_ui_cpp, "  vector<string>* %s,\n", String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_ui_cpp, "  vector<long>* %s,\n", Int1D_name[i].c_str());
	fseek(f_ui_cpp, -2, SEEK_CUR); //wind back the last , and \n
	fprintf(f_ui_cpp, ")\n");
	
	fprintf(f_ui_cpp,": UIDialog((wxWindow *) parent, id, \"%s\"),\n", MOD_name.c_str());
	for (i=0; i<Double_name.size(); i++)
	  fprintf(f_ui_cpp, "  p_%s(%s),\n", Double_name[i].c_str(), Double_name[i].c_str());
	for (i=0; i<String_name.size(); i++)
	  fprintf(f_ui_cpp, "  p_%s(%s),\n", String_name[i].c_str(), String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_ui_cpp, "  p_%s(%s),\n", Int_name[i].c_str(), Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	    fprintf(f_ui_cpp, "  p_%s(%s),\n", Double1D_name[i].c_str(), Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_ui_cpp, "  p_%s(%s),\n", String1D_name[i].c_str(), String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_ui_cpp, "  p_%s(%s),\n", Int1D_name[i].c_str(), Int1D_name[i].c_str());
	fseek(f_ui_cpp, -2, SEEK_CUR); //wind back the last , and \n
	fprintf(f_ui_cpp, "\n{\n}\n");
      }
  }while(!end_of_file);
  fclose(f_ui_cpp);
  fh_inp.close();

  printf("Done. cd %s and typ make to make lib%s.so\n", MOD_name.c_str(), MOD_name.c_str());

}
