#ifndef CFD_TEXTURE_DATA_SET_H
#define CFD_TEXTURE_DATA_SET_H
#ifdef _OSG

class cfdVolumeVisualization;
class cfdTextureManager;
#include <vector>
#include <map>
#include <iostream>
struct compareString
{
  bool operator()(const char* s1, const char* s2) const
  {
     return strcmp(s1, s2) < 0;
  }
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

   cfdTextureManager* GetActiveTextureManager();
   cfdVolumeVisualization* GetVolumeVisNode();
protected:
   unsigned int _nScalars;
   unsigned int _nVectors;
   char* _fileName;
   cfdVolumeVisualization* _volVisNode;
   cfdTextureManager* _activeTM;

   typedef std::multimap<const char*,cfdTextureManager*,compareString> TextureDataList;

   TextureDataList _scalars;
   TextureDataList _vectors;
};
#endif // _OSG
#endif// CFD_TEXTURE_DATA_SET_H
