#include <iostream>

#include "glm.h"

GLMmodel* model=NULL;

void readOBJ(char* filename);
void writeOBJ(char* filename,float epsilon,float angle);

int main(){
	char quit='n';
	char readName[100];
	char writeName[100];
	float epsilon,angle;
	while(quit=='n'){
		std::cout<<"Enter the full path of the file you want to modify."<<"  ";
		std::cin>>readName;
		std::cout<<std::endl;

		std::cout<<"Enter the maximum difference between vertices to weld. (0)"<<"  ";
		std::cin>>epsilon;
		std::cout<<std::endl;

		std::cout<<"Enter the cutoff angle for the normals to be averaged. (180)"<<"  ";
		std::cin>>angle;
		std::cout<<std::endl;

		std::cout<<"Enter the name of the file you wish to save as."<<"  ";
		std::cin>>writeName;
		std::cout<<std::endl;
	
		readOBJ(readName);
		writeOBJ(writeName,epsilon,angle);

		std::cout<<"Do you wish to quit now (y/n)?"<<"  ";
		std::cin>>quit;
      std::cout<<std::endl;
	}

	return 0;
}

void readOBJ(char* filename){
	model=glmReadOBJ(filename);
}

void writeOBJ(char* filename,float epsilon,float angle){
	char* name=filename;
	glmWeld(model,epsilon);
	glmFacetNormals(model);
	glmVertexNormals(model,angle);
	glmWriteOBJ(model,filename,GLM_SMOOTH);
}