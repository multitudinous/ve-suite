#ifdef _OSG
#include "cfdTextureDataSet.h"
#include "cfdVolumeVisualization.h"
#include "cfdTextureManager.h"
#include <iostream>
#include <fstream>
//////////////////////////////////
//Constructor                   //
//////////////////////////////////
TextureDataInfo::TextureDataInfo()
{
   _name = " ";
   _tm = 0;
}
////////////////////////////////////////////////////////////
TextureDataInfo::TextureDataInfo(const TextureDataInfo& tdi)
{
   _name = tdi._name;
   _tm = new cfdTextureManager(*tdi._tm);
}
///////////////////////////////////
TextureDataInfo::~TextureDataInfo()
{
   _name.erase();
   if(_tm){
      delete _tm;
      _tm = 0;
   }
}
///////////////////////////////////////////////
void TextureDataInfo::SetName(std::string name)
{
   _name = name;
}
//////////////////////////////////////////////////////////////
void TextureDataInfo::SetTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
}
//////////////////////////////////////
const char* TextureDataInfo::GetName()
{
   return _name.c_str();
}
///////////////////////////////////////////////////////
cfdTextureManager* TextureDataInfo::GetTextureManager()
{
   if(_tm)
   {
      return _tm;
   }
   return 0;
}
///////////////////////////////////////////////////////////////////////
TextureDataInfo& TextureDataInfo::operator=(const TextureDataInfo& tdi)
{
   if(this != &tdi){
      _name = tdi._name;
      _tm = tdi._tm;
   }
   return *this;
}
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
   
   for(unsigned int i = 0; i < _nScalars; i++)
   {
      delete _scalars.at(i);
   }
   _scalars.clear();
   for(unsigned int i = 0; i < _nVectors; i++)
   {
      delete _vectors.at(i);
   }
   _vectors.clear();
}
///////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveScalar(char* name)
{
   int whichScalar = FindScalar(name);
   if( whichScalar >= 0)
   {
      _activeTM = _scalars.at(whichScalar)->GetTextureManager();
   }
}
///////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveVector(char* name)
{
   int whichVector = FindVector(name);
   if( whichVector >= 0)
   {
      _activeTM = _vectors.at(whichVector)->GetTextureManager();
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
   if(_activeTM)
   {
      return _activeTM;
   }
   return 0;
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
      std::string tempName;
      std::getline( fin, tempName );
      std::getline( fin, tempName );
      std::cout <<"|\tScalar Name To Match VTK Dataset : "
                << tempName << std::endl;
      for(int i = 0; i < numFiles; i++)
      {         
         std::cout << "Loading texture time step file: " << i << std::endl;         
         fin >> name;         
         tm->addFieldTextureFromFile(name);      
      }

      std::cout << "Finished reading texture description file." << std::endl;
      
      if( tm->GetDataType(0) == cfdTextureManager::SCALAR )
      {
         AddScalarTextureManager( tm, tempName.c_str() );
      }
      else
      {
         AddVectorTextureManager( tm, tempName.c_str() );
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
                                                const char* scalarName)
{
   TextureDataInfo* td = new TextureDataInfo();
   td->SetName(std::string(scalarName));
   td->SetTextureManager(tm);
   _scalars.push_back(td);
   _nScalars = _scalars.size(); 
}
/////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::AddVectorTextureManager(cfdTextureManager* tm,
                                                const char* vectorName)
{
   TextureDataInfo* td = new TextureDataInfo();
   td->SetName(std::string(vectorName));
   td->SetTextureManager(tm);
   _vectors.push_back(td);
   _nVectors = _vectors.size();
}
/////////////////////////////////////////////
int cfdTextureDataSet::FindScalar(char* name)
{
   for(unsigned int i = 0; i < _nScalars; i++)
   {
      if(strstr(_scalars.at(i)->GetName(),name))
      {
         return i;
      }
   }
   return -1;
}
/////////////////////////////////////////////
int cfdTextureDataSet::FindVector(char* name)
{
   for(unsigned int i = 0; i < _nVectors; i++)
   {
      if(strstr(_vectors.at(i)->GetName(),name))
      {
         return i;
      }
   }
   return -1;
}
#endif //_OSG
