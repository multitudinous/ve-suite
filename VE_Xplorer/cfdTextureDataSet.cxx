#ifdef _OSG
#include "cfdTextureDataSet.h"
#include "cfdVolumeVisualization.h"
#include "cfdTextureManager.h"
#include <iostream>
#include <fstream>
//////////////////////////////////////
//Constructor                       //
//////////////////////////////////////
cfdTextureDataSet::cfdTextureDataSet()
{
   _fileName = 0;
   _volVisNode = new cfdVolumeVisualization();
   _nScalars = 0;
   _nVectors = 0;
   _activeTM = 0;
}
///////////////////////////////////////
//Destructor                         //
///////////////////////////////////////
cfdTextureDataSet::~cfdTextureDataSet()
{
   if(_fileName){
      delete [] _fileName;
      _fileName = 0;
   }
   if(_volVisNode){
      delete _volVisNode;
      _volVisNode = 0;
   }
   TextureDataList::iterator pos;

   //need to fix this, I'm sure it's a memory leak. . .
   for (pos = _scalars.begin( ); pos != _scalars.end( ); pos++)
   {
      _scalars.erase( pos );
   }
   
   for ( pos = _vectors.begin(); pos != _vectors.end(); )
   {
      _vectors.erase( pos++ );
   }
}
///////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveScalar(char* name)
{
   TextureDataList::iterator pos;
   for (pos = _scalars.begin(); 
       pos != _scalars.end(); 
       ++pos)
   {
      std::cout<<"Scalar :" <<(*pos).first<<std::endl;
      if(strstr(pos->first,name)){
         _activeTM = pos->second;
         break;
      }
   }
}
///////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveVector(char* name)
{
   TextureDataList::iterator pos;
   for (pos = _vectors.begin( ); pos != _vectors.end( ); pos++)
   {
      std::cout<<"Vector :" <<pos->first<<std::endl;
      if(strstr(pos->first,name)){
         _activeTM = (*pos).second;
         break;

      }
   }
}
///////////////////////////////////////////////
void cfdTextureDataSet::SetFileName(char* name)
{
   if(_fileName){
      delete [] _fileName;
      _fileName = 0;
   }
   _fileName = new char[strlen(name)+1];
   strcpy(_fileName,name);
}
/////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureDataSet::GetVolumeVisNode()
{
   if(_volVisNode){
      if(_activeTM){
         _volVisNode->SetTextureManager(_activeTM);
      }
      return _volVisNode;
   }
   return 0;
}
////////////////////////////////////////////////////////////////
cfdTextureManager* cfdTextureDataSet::GetActiveTextureManager()
{
   return _activeTM;
}
//////////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::CreateTextureManager(char* textureDescriptionFile)
{
   cfdTextureManager* tm = new cfdTextureManager();
   std::ifstream fin( textureDescriptionFile );   
   char name[256];
   
   if ( fin.is_open() )
   {       
      std::cout << "Reading texture description file: " 
                  << textureDescriptionFile << std::endl;
      int numFiles = 0;      
      fin >> numFiles;      

      for(int i = 0; i < numFiles; i++)
      {         
         std::cout << "Loading texture time step file: " << i << std::endl;         
         fin >> name;         
         tm->addFieldTextureFromFile(name);      
      }

      std::cout << "Finished reading texture description file." << std::endl;
      
      if( tm->GetDataType(0) == cfdTextureManager::SCALAR )
      {
         AddScalarTextureManager( tm, textureDescriptionFile );
      }
      else
      {
         AddVectorTextureManager( tm, textureDescriptionFile );
      }
   }
   else
   {
      std::cout << "Couldn't open file in cfd3DTextureBasedModel::CreateTextureManager!" << std::endl;
      return;
   }
}
/////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::AddScalarTextureManager(cfdTextureManager* tm,
                                                char* scalarName)
{
   char* lName = new char[strlen(scalarName)+1];
   strcpy(lName,scalarName);
   _scalars.insert( std::pair<const char* , 
                           cfdTextureManager*>(lName, tm));
}
/////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::AddVectorTextureManager(cfdTextureManager* tm,
                                                char* vectorName)
{
   char* lName= new char[strlen(vectorName)+1];
   strcpy(lName,vectorName);
   _vectors.insert( std::pair<const char* , 
                           cfdTextureManager*>(lName, tm));
}
#endif //_OSG
