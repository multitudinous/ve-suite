#ifndef CFD_TEXTURE_DATA_SET_H
#define CFD_TEXTURE_DATA_SET_H
#ifdef _OSG

class cfdVolumeVisualization;
class cfdTextureManager;
#include <vector>
#include <map>
#include <iostream>
#include <string>

class TextureDataInfo{
public:
   TextureDataInfo();
   TextureDataInfo(const TextureDataInfo& tdi);
   ~TextureDataInfo();
   void SetName(std::string name);
   void SetTextureManager(cfdTextureManager* tm);

   const char* GetName();
   cfdTextureManager* GetTextureManager();
   TextureDataInfo& operator=(const TextureDataInfo& tdi);
protected:
   std::string _name;
   cfdTextureManager* _tm;

};

class cfdTextureDataSet{
public:
   cfdTextureDataSet();
   virtual ~cfdTextureDataSet();

   void SetActiveScalar(char* name);
   void SetActiveVector(char* name);
   void SetFileName(char* name);
   void CreateTextureManager(char* textureDescriptionFile);
   void AddScalarTextureManager( cfdTextureManager*, char* );
   void AddVectorTextureManager( cfdTextureManager*, char* );

   int FindVector(char* name);
   int FindScalar(char* name);

   cfdTextureManager* GetActiveTextureManager();
   cfdVolumeVisualization* GetVolumeVisNode();
protected:
   unsigned int _nScalars;
   unsigned int _nVectors;
   char* _fileName;
   cfdVolumeVisualization* _volVisNode;
   cfdTextureManager* _activeTM;

   typedef std::vector<TextureDataInfo*> TextureDataList;

   std::vector<std::string> _scalarNames;
   std::vector<std::string> _vectorNames;

   TextureDataList _scalars;
   TextureDataList _vectors;
};
#endif // _OSG
#endif// CFD_TEXTURE_DATA_SET_H
