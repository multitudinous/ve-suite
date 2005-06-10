#ifdef VE_PATENTED
#ifdef _OSG
#include "VE_VolumeVis/cfdTextureDataSet.h"
#include "VE_VolumeVis/cfdVolumeVisualization.h"
#include "VE_VolumeVis/cfdTextureManager.h"
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
   _activeDataType = SCALAR;
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
//////////////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveScalar(const char* name)
{
   int whichScalar = FindScalar(name);
   if( whichScalar >= 0)
   {
      _activeTM = _scalars.at(whichScalar)->GetTextureManager();
      _activeDataType = SCALAR;
   }
}
//////////////////////////////////////////////////////////
void cfdTextureDataSet::SetActiveVector(const char* name)
{
   int whichVector = FindVector(name);
   if( whichVector >= 0)
   {
      _activeTM = _vectors.at(whichVector)->GetTextureManager();
      _activeDataType = VECTOR;
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
/////////////////////////////////////////////////
unsigned int cfdTextureDataSet::NumberOfScalars()
{
   return _nScalars;
}
/////////////////////////////////////////////////
unsigned int cfdTextureDataSet::NumberOfVectors()
{
   return _nVectors;
}
/////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureDataSet::GetVolumeVisNode()
{
   if(_volVisNode){
      if(_activeTM){
         _volVisNode->SetBoundingBox(_activeTM->getBoundingBox());
      }
      return _volVisNode;
   }
   return 0;
}
///////////////////////////////////////////////////////////////
cfdTextureDataSet::DataType cfdTextureDataSet::ActiveDataType()
{
   return _activeDataType;
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
//////////////////////////////////////////////////////////////
const char* cfdTextureDataSet::ScalarName(unsigned int index)
{
   return _scalarNames.at(index).c_str();
}
/////////////////////////////////////////////////////////////
const char* cfdTextureDataSet::VectorName(unsigned int index)
{
   return _vectorNames.at(index).c_str();
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureDataSet::CreateTextureManager(const char* textureDescriptionFile)
{
   cfdTextureManager* tm = new cfdTextureManager();
   tm->SetUseShaders(true);
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
      std::cout<<textureDescriptionFile<<std::endl;
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
   _activeTM = tm;
   _scalarNames.push_back(std::string(scalarName));
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
   _vectorNames.push_back(std::string(vectorName));
}
///////////////////////////////////////////////////
int cfdTextureDataSet::FindScalar(const char* name)
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
///////////////////////////////////////////////////
int cfdTextureDataSet::FindVector(const char* name)
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
#endif
#endif
