/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif
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
//generate UI Plugin code
void GenCode_UI_Plugin();
void GenVcProj_UI_Plugin( const char* defname ); //generates the vcproj file for use in Windows
//generate Graphical Plugin Code
void GenCode_Graphical_Plugin();
void GenVcProj_Graphical_Plugin( const char* defname );
//generate Unit code
void GenCode_Unit();
void GenVcProj_Unit( const char* defname );

int main(int argc, char* argv[]) //This is a Cheezy wizard. take a mod.def as input
{
  if (argc<2)
    printf("The Format is ModWiz [module definition file]\n");
  else
    {
      parse_inp(argv[1]);
      GenCode_UI_Plugin();
      GenVcProj_UI_Plugin( argv[1] );
      GenCode_Unit();
      //generate vcproj for Unit at this spot
      GenCode_Graphical_Plugin();
      GenVcProj_Graphical_Plugin( argv[1] );
    }
}
/////////////////////////////////////////////////////
//                UI PLUGIN CODE GENERATION        //
/////////////////////////////////////////////////////

void GenCode_Unit()
{
   string unitName;
   char buffer[BUFFER_MAX+1];
   unsigned int pos;	//returns position of search string
   int counter;

   cout<<"Generating Unit Makefile ...";
   ofstream outUnitFile;
   string bufferString;
   unitName = MOD_name + "Unit";
   //cout<<"UNIT NAME :"<<unitName<<endl;
#ifdef WIN32
   _mkdir( unitName.c_str());
   _chdir( unitName.c_str() );
#else
   mkdir( unitName.c_str(), S_IRWXU|S_IRWXG );
   chdir( unitName.c_str() );
#endif
   outUnitFile.open( "Makefile" );
   //generate Makefile for Unit
   ifstream inUnitFile;
   inUnitFile.open( "../../Makefile.Unit.Template" );
   while ( !inUnitFile.eof() )
   {   
      //store each line in the buffer       
      inUnitFile.getline( buffer, BUFFER_MAX, '\n' );
      bufferString += buffer;
      //now find the position of the word "Template"
      pos = bufferString.find( "Template" );
      if ( pos != string::npos )
      {
	 counter++;
	 bufferString.replace( pos, 8, unitName );
      }
      outUnitFile<<bufferString<<endl;
      bufferString.clear();
   }
   outUnitFile.close();
   inUnitFile.close();
   cout<<" Done"<<endl;
   //now generate Unit code
   inUnitFile.clear();
   inUnitFile.open( "../../TEMPLATE_i.h" );
   outUnitFile.open( (unitName+"_i.h").c_str() );  //open the _i.h files
   cout<<"Generating "<<unitName+"_i.h";
   while ( !inUnitFile.eof() )
   {   
      //store each line in the buffer       
      inUnitFile.getline( buffer, BUFFER_MAX, '\n' );
      bufferString += buffer;
      //now find the position of the word "Template"
      pos = bufferString.find( "Template" );
      if ( pos != string::npos )
      {
	 counter++;
	 bufferString.replace( pos, 8, unitName );
      }
      outUnitFile<<bufferString<<endl;
      bufferString.clear();
   }
   cout<<"... Done"<<endl;
   inUnitFile.close();
   outUnitFile.close();
   inUnitFile.clear();
   inUnitFile.open( "../../TEMPLATE_i.cpp" );
   outUnitFile.open( (unitName+"_i.cpp").c_str() );  //open the _i.h files
   cout<<"Generating "<<unitName+"_i.cpp";
   while ( !inUnitFile.eof() )
   {   
      //store each line in the buffer       
      inUnitFile.getline( buffer, BUFFER_MAX, '\n' );
      bufferString += buffer;
      //now find the position of the word "Template"
      pos = bufferString.find( "Template" );
      if ( pos != string::npos )
      {
	 counter++;
	 bufferString.replace( pos, 8, unitName );
      }
      outUnitFile<<bufferString<<endl;
      bufferString.clear();
   }
   cout<<"... Done"<<endl;
   inUnitFile.close();
   outUnitFile.close();
   inUnitFile.clear();
   inUnitFile.open( "../../TEMPLATE_client.cpp" );
   outUnitFile.open( (unitName+"_client.cpp").c_str() );  //open the _i.h files
   cout<<"Generating "<<unitName+"_client.cpp";
   while ( !inUnitFile.eof() )
   {   
      //store each line in the buffer       
      inUnitFile.getline( buffer, BUFFER_MAX, '\n' );
      bufferString += buffer;
      //now find the position of the word "Template"
      pos = bufferString.find( "Template" );
      if ( pos != string::npos )
      {
         counter++;
         if ( counter == 2 ) bufferString.replace( pos, 8, MOD_name );
         else bufferString.replace( pos, 8, unitName );
         
      }
      outUnitFile<<bufferString<<endl;
      bufferString.clear();
   }
   cout<<"... Done"<<endl;
   inUnitFile.close();
   outUnitFile.close();

   chdir( "../" );
}

void GenCode_Graphical_Plugin()
{
   cout<<"Generating Graphical Plugin Makefile...";
   string graphicalPluginName;
   
   char buffer[BUFFER_MAX+1];
   unsigned int pos;	//returns position of search string
   int counter;
   ofstream outGraphicalPlugin;
   string bufferString;
   graphicalPluginName = MOD_name + "GraphicalPlugin";
   //cout<<"UNIT NAME :"<<graphicalPluginName<<endl;
#ifdef WIN32
   _mkdir( graphicalPluginName.c_str());
   _chdir( graphicalPluginName.c_str());
#else
   mkdir( graphicalPluginName.c_str(), S_IRWXU|S_IRWXG );
   chdir( graphicalPluginName.c_str() );
#endif
   outGraphicalPlugin.open( "Makefile" );
   //generate Makefile for Graphical plugin
   ifstream graphicalPluginFiles;
   graphicalPluginFiles.open( "../../Makefile.GraphicalPlugin.TEMPLATE" );
   while ( !graphicalPluginFiles.eof() )
   {   
      //store each line in the buffer       
      graphicalPluginFiles.getline( buffer, BUFFER_MAX, '\n' );       
      bufferString += buffer;
      //now find the position of the word "Template"
      pos = bufferString.find( "Template" );
      if ( pos !=string::npos )
      {
         counter++;
         bufferString.replace( pos, 8, graphicalPluginName );
      }
      outGraphicalPlugin<<bufferString<<endl;
      bufferString.clear();
   }   
   outGraphicalPlugin.close();
   graphicalPluginFiles.close();
   graphicalPluginFiles.clear();
   graphicalPluginFiles.open( "../../VETEMPLATE.h" );
   string ve = "VE";
   outGraphicalPlugin.open( (ve + graphicalPluginName+".h").c_str() );
   while ( !graphicalPluginFiles.eof() )
   {   
      //store each line in the buffer       
      graphicalPluginFiles.getline( buffer, BUFFER_MAX, '\n' );       
      bufferString += buffer;
      //now find the position of the word "Template"
      pos = bufferString.find( "Template" );
      if ( pos !=string::npos )
      {
	 counter++;
	 bufferString.replace( pos, 8, graphicalPluginName );
      }
      outGraphicalPlugin<<bufferString<<endl;
      bufferString.clear();
   }   
   outGraphicalPlugin.close();
   graphicalPluginFiles.close();
   graphicalPluginFiles.clear();
   graphicalPluginFiles.open( "../../VETEMPLATE.cpp" );
   outGraphicalPlugin.open( (ve + graphicalPluginName+".cpp").c_str() );
   counter = 0;
   while ( !graphicalPluginFiles.eof() )
   {
      //store each line in the buffer       
      graphicalPluginFiles.getline( buffer, BUFFER_MAX, '\n' );       
      bufferString += buffer;
      //now find the position of the word "Template"
      for ( int ct=0;ct<2;ct++ )
      {
         pos = bufferString.find( "Template" );
         if ( pos !=string::npos )
         {
            counter++;
            if ( counter == 4 ) bufferString.replace( pos, 8, MOD_name );
            else bufferString.replace( pos, 8, graphicalPluginName );
         }
         
         pos = 0;
      }
      outGraphicalPlugin<<bufferString<<endl;
      bufferString.clear();
   }
   outGraphicalPlugin.close();
   graphicalPluginFiles.close();
   graphicalPluginFiles.clear();
   cout<<" Done"<<endl;
}
void GenVcProj_Graphical_Plugin( const char* defname )
{
	cout<<"generating vcproj file for Graphical Plugin...";
	char buffer[BUFFER_MAX+1];
	string vcprojName;
	string gPluginName;
	string modDefName;
	ifstream inpVcproj( "../../TEMPLATEGraphicalPlugin.vcproj" );
	vcprojName = defname;   //initialize the name of the project to that given by the user
	unsigned int pos = vcprojName.find( ".def" );
	vcprojName.erase( pos, 4 );
	modDefName = vcprojName;
	gPluginName = modDefName + "GraphicalPlugin";
	vcprojName = vcprojName + "GraphicalPlugin.vcproj";
	ofstream outVcproj ( vcprojName.c_str() );
	string bufferString;
	int counter;
	while ( !inpVcproj.eof() )
    {
      inpVcproj.getline(buffer, BUFFER_MAX, '\n');
      bufferString += buffer;      
      //within each line look for Template and replace with  modDefName
	  pos = bufferString.find( "TEMPLATE" );
      if ( pos !=string::npos )
      {
         counter++;
         bufferString.replace( pos, 8, gPluginName );
      }
      outVcproj<<bufferString<<endl;
      
      bufferString.clear();
    }//finished reading end of files
	cout<<" Done"<<endl;
	chdir( "../" );
}

void GenVcProj_UI_Plugin( const char* defname )
{
   cout<<"Generating vcproj file...";
   char buffer[BUFFER_MAX+1];
   //read in an existing vcproj file i.e. TEMPLATE.vcproj
   string vcprojName;   //name of the vcproj
   string unitName;	//name of files in Unit
   
   //open TEMPLATE.vcproj   
   string modDefName;   //string to hold the model definition names
   ifstream inpVcproj( "../../TEMPLATE.vcproj" );   
   vcprojName = defname;   //initialize the name of the project to that given by the user
   
   unsigned int pos = vcprojName.find( ".def" );
   vcprojName.erase( pos, 4 );
   modDefName = vcprojName;
   vcprojName = vcprojName + ".vcproj";
   cout<<vcprojName<<endl;
   ofstream outVcproj ( vcprojName.c_str() );   
   //read in each line
   string bufferString;
   int counter; counter = 0;
   while ( !inpVcproj.eof() )
   {
      inpVcproj.getline(buffer, BUFFER_MAX, '\n');
      bufferString += buffer;      
      //within each line look for Template and replace with  modDefName
      pos = bufferString.find( "Template" );
      if ( pos !=string::npos )
      {
         counter++;
         bufferString.replace( pos, 8, modDefName );
      }
      outVcproj<<bufferString<<endl;
      
      bufferString.clear();
   }//finished reading end of files
   //cout<<counter<<" occurances of Template found"<<endl;
   //go back one directory up
   
   chdir("../");
}


//==========================================
void GenCode_UI_Plugin()
{
  string mod_h;
  string mod_cpp;
  string mod_ui_h;
  string mod_ui_cpp;
  //strings for directory names
  string UI_Plugin;//to store the directory name for the UI Plugin
  char buffer[BUFFER_MAX+1];
  char buffer2[BUFFER_MAX+1];
  char out_buf[BUFFER_MAX+1];

  mod_h = MOD_name+".h";
  mod_cpp = MOD_name+".cpp";
  mod_ui_h = MOD_name+"_UI_Dialog.h";
  mod_ui_cpp = MOD_name+"_UI_Dialog.cpp";
  UI_Plugin = MOD_name + "UIPlugin";

  printf("Make new directory %s\n", MOD_name.c_str());

#ifdef WIN32
  _mkdir(MOD_name.c_str());
  _chdir(MOD_name.c_str());
  _mkdir(UI_Plugin.c_str());
  _chdir(UI_Plugin.c_str());
#else
  mkdir(MOD_name.c_str(), S_IRWXU|S_IRWXG);
  chdir(MOD_name.c_str());
  mkdir(UI_Plugin.c_str(), S_IRWXU|S_IRWXG);
  chdir(UI_Plugin.c_str());
#endif
  //no more file copies
  //system("cp ../Makefile.incl ./");
  //system("cp ../interface.h ./");
  //system("cp ../package.h ./");
  //system("cp ../string_ops.h ./");
  //system("cp ../Plugin_base.h ./");
  //system("cp ../UIDialog.h ./");
  ifstream fh_inp("../../TEMPLATE.h");
  ifstream fcpp_inp("../../TEMPLATE.cpp");
  ifstream fuih_inp("../../TEMPLATE_UI_Dialog.h");
  ifstream fuicpp_inp("../../TEMPLATE_UI_Dialog.cpp");
  ifstream fmake_inp("../../Makefile.TEMPLATE");
  FILE *f_h = fopen(mod_h.c_str(), "w");
  FILE *f_cpp = fopen(mod_cpp.c_str(), "w");
  FILE *f_ui_h = fopen(mod_ui_h.c_str(), "w");
  FILE *f_ui_cpp = fopen(mod_ui_cpp.c_str(), "w");
  FILE *f_make = fopen("Makefile", "w");
  char* pos_temp;
  char* if_public;
  char* if_constructor;
  char* if_UIDialog;
  char* if_UIDialog_include;
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
	  fprintf(f_h, "  std::string %s;\n", String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_h, "  long %s;\n", Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	  fprintf(f_h, "  std::vector< double > %s;\n", Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_h, "  std::vector< std::string > %s;\n", String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_h, "  std::vector< long > %s;\n", Int1D_name[i].c_str());
	
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
   {
      fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Double_name[i].c_str(), Double_name[i].c_str());
      fprintf(f_cpp, "  \%s = 0.0f;\n", Double_name[i].c_str() );
   }
	for (i=0; i<String_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", String_name[i].c_str(), String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
   {
      fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Int_name[i].c_str(), Int_name[i].c_str());
      fprintf(f_cpp, "  \%s = 0;\n", Int_name[i].c_str() );
   }
	for (i=0; i<Double1D_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Double1D_name[i].c_str(), Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", String1D_name[i].c_str(), String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_cpp, "  RegistVar(\"%s\", &%s);\n", Int1D_name[i].c_str(), Int1D_name[i].c_str());
	fprintf(f_cpp, "}\n");
      }

    if_UIDialog = strstr(buffer2, "TEMPLATE_UI_Dialog");
    // We needto make sure that we only change the constructor and 
    // not the include spec at the top of the TEMPPLATE.cpp file
    if (if_UIDialog && !strstr(buffer2, "TEMPLATE_UI_Dialog.h") )
      {
   // This is for the first TEMPLATE_UI_Dialog, which is the include 
   
   // This is for the second TEMPLATE_UI_Dialog, line 92 of TEMPLATE.cpp
	fseek(f_cpp, -2, SEEK_CUR); // wind back 1 space and \n
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
	  fprintf(f_ui_h, "  std::string* p_%s;\n", String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_ui_h, "  long* p_%s;\n", Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	  fprintf(f_ui_h, "  std::vector< double >* p_%s;\n", Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_ui_h, "  std::vector< std::string >* p_%s;\n", String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_ui_h, "  std::vector< long >* p_%s;\n", Int1D_name[i].c_str());
	
      }
      else
	{
	  //First pulbic, should print the constructor with the GUI variables
	  fprintf(f_ui_h, "  %s_UI_Dialog(wxWindow* parent, int id,\n", MOD_name.c_str());
   
	  for (i=0; i<Double_name.size(); i++)
	    fprintf(f_ui_h, "          double* %s,\n", Double_name[i].c_str());
	  for (i=0; i<String_name.size(); i++)
	    fprintf(f_ui_h, "          std::string* %s,\n", String_name[i].c_str());
	  for (i=0; i<Int_name.size(); i++)
	    fprintf(f_ui_h, "          long* %s,\n", Int_name[i].c_str());
	  for (i=0; i<Double1D_name.size(); i++)
	    fprintf(f_ui_h, "          std::vector< double >* %s,\n", Double1D_name[i].c_str());
	  for (i=0; i<String1D_name.size(); i++)
	    fprintf(f_ui_h, "          std::vector< std::string >* %s,\n", String1D_name[i].c_str());
	  for (i=0; i<Int1D_name.size(); i++)
	    fprintf(f_ui_h, "          std::vector< long >* %s,\n", Int1D_name[i].c_str());
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
	  fprintf(f_ui_cpp, "  std::string* %s,\n", String_name[i].c_str());
	for (i=0; i<Int_name.size(); i++)
	  fprintf(f_ui_cpp, "  long* %s,\n", Int_name[i].c_str());
	for (i=0; i<Double1D_name.size(); i++)
	    fprintf(f_ui_cpp, "  std::vector< double >* %s,\n", Double1D_name[i].c_str());
	for (i=0; i<String1D_name.size(); i++)
	  fprintf(f_ui_cpp, "  std::vector< std::string >* %s,\n", String1D_name[i].c_str());
	for (i=0; i<Int1D_name.size(); i++)
	  fprintf(f_ui_cpp, "  std::vector< long >* %s,\n", Int1D_name[i].c_str());
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
	  printf("Read in Double var: %s \n", toks[1].c_str());
	  break;
	case 3:
	  String_name.push_back(toks[1]);
	  printf("Read in String var: %s \n", toks[1].c_str());
	  break;
	case 4:
	  Int_name.push_back(toks[1]);
	  printf("Read in Int var: %s \n", toks[1].c_str());
	  break;
	case 5:
	  Double1D_name.push_back(toks[1]);
	  printf("Read in Double1D var: %s \n", toks[1].c_str());
	  break;
	case 6:
	  String1D_name.push_back(toks[1]);
	  printf("Read in String1D var: %s \n", toks[1].c_str());
	  break;
	case 7:
	  Int1D_name.push_back(toks[1]);
	  printf("Read in Int1D var: %s \n", toks[1].c_str());
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
    {
      printf("%s is not a Key word\n");
      return 0; //not a key word
    }
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
