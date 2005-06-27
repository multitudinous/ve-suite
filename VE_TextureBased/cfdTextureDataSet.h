#ifndef CFD_TEXTURE_DATA_SET_H
#define CFD_TEXTURE_DATA_SET_H
#ifdef _OSG
#ifdef VE_PATENTED
namespace VE_TextureBased{
   class cfdVolumeVisualization;
   class cfdTextureManager;
}
#include <vector>
#include <map>
#include <iostream>
#include <string>
#include "VE_Installer/VEConfig.h"
namespace VE_TextureBased{
   class VE_TEXTURE_BASED_EXPORTS TextureDataInfo{
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

   class VE_TEXTURE_BASED_EXPORTS cfdTextureDataSet{
      public:
         cfdTextureDataSet();
         virtual ~cfdTextureDataSet();

         enum DataType {SCALAR,VECTOR};

         void SetActiveScalar(const char* name);
         void SetActiveVector(const char* name);
         void SetFileName(char* name);
         void CreateTextureManager(const char* textureDescriptionFile);
         void AddScalarTextureManager( cfdTextureManager*, const char* );
         void AddVectorTextureManager( cfdTextureManager*, const char* );

         int FindVector(const char* name);
         int FindScalar(const char* name);

         unsigned int NumberOfScalars();
         unsigned int NumberOfVectors();

         const char* ScalarName(unsigned int index);
         const char* VectorName(unsigned int index);
   
         DataType ActiveDataType();

         cfdTextureManager* GetActiveTextureManager();
         cfdVolumeVisualization* GetVolumeVisNode();
      protected:
         DataType _activeDataType;
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
}
#endif
#endif
#endif// CFD_TEXTURE_DATA_SET_H
