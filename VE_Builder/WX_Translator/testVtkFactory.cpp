#include "fluentIO.h"
#include "fluentCase.h"
#include "fluentVtkFactory.h"
#include <string>
#include <vector>
#include <iostream> 
#include <fstream> 
#include <sstream>

bool StringToInt(const std::string &s, int &i)
{
  std::istringstream myStream(s);
  
  if (myStream>>i)
    return true;
  else
    return false;
}

void writeFile(std::string casefile, std::string datafile, 
        std::vector<int> scalar_ids, std::vector<std::string> scalar_names, 
        std::vector<int> vector_ids, std::string vector_name,
        bool isBinary = true, bool isGzip = false )
{   
    
    std::string info = casefile + ".info";
    std::string check; // = casefile + ".check";
    //check = "";    
    cout << "check size = " << check.size() << endl;
    FluentReader::FluentIO *infile1 = new FluentReader::FluentIO(
        casefile, info, check, isBinary, isGzip);  
    //infile1->enableDouble();
    infile1->enableFloat();

    FluentReader::Case *case1 = new FluentReader::Case(infile1);
    case1->read();
    delete(infile1);  
    case1->toGlobalFaceConnect();
    case1->toGlobalCellConnect();

    //check = datafile + ".check";
    info = datafile + ".info";
    cout << "check size = " << check.size() << endl;

    FluentReader::FluentIO *infile2 = new FluentReader::FluentIO(
        datafile, info, check, isBinary, isGzip);
    
    //FLOAT OR DOUBLE
    //infile2->enableDouble();
    infile2->enableFloat();
    
    FluentReader::Case *data1 = new FluentReader::Case(infile2);
    data1->read();
    delete(infile2);
 

    FluentReader::VtkFactory *vtk_factory = new FluentReader::VtkFactory( case1, data1);
    vtk_factory->enableOutput();
    vtk_factory->addCellConnect();
    vtk_factory->addPoints();
    vtk_factory->addScalars( scalar_ids, scalar_names );
    
    //Line is necessary for vector data 
    //commented out because Virtual Future Gen Power Plant doesn't need vector data
    //vtk_factory->addVector( vector_ids, vector_name ); 
    
    std::string outfile = datafile + "_2.vtk";

    //Added by Alberto Jove 12/06/2004
	std::string tempName;
	int pos;

	pos=outfile.find_last_of("/",1000);
	tempName.assign(outfile);
	tempName = outfile.substr(pos,100);
	outfile.assign("converted_files");
	outfile.append(tempName);
    //
    vtk_factory->addParentFlag();
    vtk_factory->toFile( outfile );
}

int main(int argc, char* argv[])
{   
	//DAVES ORIGINAL CODE///////////////////////////////////////////////////
	//int a[] = {1,101,4,3,200,201,202,203,204};
	//int b[]= {111,112,113};
	//char *c[] = {"p","rho","h","T","CH4","O2","H2O","CO2","N2"};
	//int nScalar = 8;
	//int nVector = 3;
	//std::vector<int> scalar_ids(nScalar);
	//std::vector<int> vector_ids(nVector);
	//std::vector< std::string > scalar_names(nScalar);
	//std::string vector_name = "velocity";

	//for (int i = 0; i < nScalar; i++) 
	//{
	//    scalar_ids[i] = a[i];
	//    scalar_names[i] = c[i];
	//    std::cout << scalar_ids[i] << " " << scalar_names[i] << std::endl;
	//}
	//vector_ids[0] = b[0];
	//vector_ids[1] = b[1];
	//vector_ids[2] = b[2];
	//////////////////////////////////////////////////////////////////////////
	
/*	char fileName[strlen(argv[1])+14];
	ifstream scalar_file, vector_file;
	int sToI;
	
	//SCALARS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	scalar_file.open(strcat(strcpy(fileName,argv[1]), ".scalars.param"), ifstream::in); 
	std::string  temp, temp2;
	std::vector<int> scalar_ids;
	std::vector<std::string> scalar_names;
	
	//read in scalar param file
	scalar_file >> temp;
	while ( !scalar_file.eof( ) ) 
	{ 
		//read scalar names into a vector
		scalar_names.push_back(temp);
		//read in scalar id values and convert
		scalar_file >> temp2;
		//temp3 = temp2.c_str();
		
		//read scalar_ids into a vector
		//scalar_ids.push_back(atoi(temp3));	
		if(!StringToInt(temp2, sToI))
		{	cout<<"Invalid scalar param file format.";
			return 1;
		}
		scalar_ids.push_back(sToI);
		scalar_file >> temp;	
	}
	for(int i =0; i<scalar_ids.size(); i++)
		cout << scalar_ids[i]<<endl;
	scalar_file.close();
	
	//ONCE AGAIN VECTORS COMMENTED OUT NOT NECESSARY FOR VFGPP
	//VECTORS!!!!!!!!!!!!!!!!!!!!!!
	//create for vectors filename and initialize	
//	vector_file.open(strcat(strcpy(fileName,argv[1]),".vectors.param"), ifstream::in);
//	std::string  vtemp;
//	const char * vtemp2;
	std::vector<int> vector_ids;
	std::string vector_name;
	
	//read the vector param file
//	vector_file >> vector_name;
	//read in vectors
//	vector_file >> vtemp;
//	while ( !vector_file.eof( ) ) 
//	{ 
//		//read in vector id values and convert
//		//vtemp2 = vtemp.c_str();
//		
//		//read scalar_ids into a vector
//		//vector_ids.push_back(atoi(vtemp2));
//		if(!StringToInt(vtemp, sToI))
//		{	cout<<"Invalid scalar param file format.";
//			return 1;
//		}		
//		vector_ids.push_back(sToI);
//		vector_file >> vtemp;
//	}
//	for(int i =0; i<vector_ids.size(); i++)
//		cout << vector_ids[i]<<endl;
//	vector_file.close();
*/

	//create case and data filenames
	//char casFile[strlen(argv[1])+4];
	//char datFile[strlen(argv[1])+4];
	char * casFile;
	casFile = (char*) malloc(strlen(argv[1])+4);
	char * datFile;
	datFile = (char*) malloc(strlen(argv[1])+4);

	strcpy(casFile, argv[1]);
	strcat(casFile, ".cas");
	strcpy(datFile, argv[1]);
	strcat(datFile, ".dat");   
	//std::cout<<"The value of argv[1]: "<<argv[1]<<std::endl;
	////FLUENT PARSER - RETRIEVES SCALARS FROM FLUENT FILE
	char buffer[100000];
	char tempBuffer[100000];
	int varNum;
	std::string theName;
	std::string theValue;
	std::vector<std::string> scalar_names;
	//std::vector<std::string> temp_scalar_ids;
	std::vector<int> scalar_ids;
	//ifstream OpenFile(datFile);
	ifstream OpenFile(datFile, ios::binary|ios::in); //fix for windows
	char ch;
	int count = 0;
	bool foundFlag = false;
	
	//parse entire dat file
	//Added by Alberto Jove - 22/11/2004
	std::string line, name;
	ifstream readFile;
	name = "parser_file/cmdLineFile.txt";
	readFile.open(name.data());
	assert(readFile.is_open());
	while(!readFile.eof())
	{
		readFile.getline(buffer, 100000);
		line.assign(buffer);
		if(!line.empty())
		{
			//std::cout<<"The value of line is: "<<buffer<<std::endl;
			theName.assign(strtok(buffer, " "));
			theValue.assign(strtok(NULL,"\n"));
			scalar_names.push_back(theName);
			scalar_ids.push_back(atoi(theValue.c_str()));
			//std::cout<<"The value var: "<<var<<std::endl;
			//std::cout<<"The value id: "<<id<<std::endl;
		}
	}
	readFile.close();
	
	//
	
	/*while(!OpenFile.eof())
	{
		OpenFile.getline(buffer, 100000);

		//look for a valid value
		//std::cout<<"Going to look for a valid value!"<<std::endl;
		
		if(strncmp(buffer, "(3300 (",7)==0)
		{	
			
			//get variable name
			strtok(tempBuffer, "\"");
			theName.assign(strtok(NULL, ","));
			
			//search for a duplicate entry
			for(int i = 0; i < scalar_names.size(); i++)
			{
				if(strcmp(theName.c_str(), scalar_names.at(i).c_str()) == 0)
				{
					//a duplicate value located
					foundFlag = true;
					//exit for loop
					i = scalar_names.size();
				}
				else
					//theName is a unique 
					foundFlag = false;
			}	
			if(!foundFlag)			
			{
				scalar_names.push_back(theName);
			
				//get variable number
				strtok(buffer, "(");
				theValue.assign(strtok(NULL, " "));
				//temp_scalar_ids.push_back(theValue.c_str());
				scalar_ids.push_back(atoi(theValue.c_str()));
			}
		}
		
		//used to obtain prior line	
		strcpy(tempBuffer, buffer);
	}*/
	//write out param files
	//ofstream SaveFile(argv[2]);
	//for(int i = 0; i < scalar_names.size(); i++)
	//{
	//	SaveFile.write(scalar_names.at(i).c_str(),  scalar_names.at(i).size());
	//	SaveFile.put(' ');
	//	SaveFile.write(temp_scalar_ids.at(i).c_str(), temp_scalar_ids.at(i).size());
	//	SaveFile.put('\n');
	//}
	
	//NULL vector of VECTORS
	std::vector<int> vector_ids;
	std::string vector_name;
    
	//translate the data to vtk
	writeFile(casFile,datFile,scalar_ids, scalar_names, vector_ids, vector_name);
    
    return 0; 
}
