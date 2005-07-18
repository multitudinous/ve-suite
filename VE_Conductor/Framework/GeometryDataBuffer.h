#ifndef GEOMETRY_DATABUFFER_H
#define GEOMETRY_DATABUFFER_H

#include <fstream>
#include <vector>
#include <map>
class GeometryInfoPackage
{
   public:
      GeometryInfoPackage();
      ~GeometryInfoPackage();
      
      void SetGeomName(std::string geomname);
      void SetModelType(int type);
      void SetGeomFileName(std::string filename);
      void SetTransparencyToggle(bool);
      void SetScales(double,double,double);
      void SetTrans(double,double,double);
      void SetRots(double,double,double);
      void SetColors(double,double,double);
      void SetColorFlag(bool flag);
      void SetLOD(double lod);
      
      std::string GetGeomName();
      int GetModelType();
      std::string GetGeomFileName();
      bool GetTransparencyToggle();
      double* GetScales();
      double* GetTrans();
      double* GetRots();
      double* GetColors();
      bool GetColorFlag();
      double GetLOD();
      
   private:
      int modeltype;
      std::string geomname;
      std::string geomfilename;
      bool transparencytoggle; 
      double scales[3];
      double trans[3];
      double rots[3];
      bool colorflag;
      double colors[3];
      double LOD;

         
};

//A simple class to contain the geom info
/*typedef struct
{
   int modeltype;
   std::string geomname;
   std::string geomfilename;
   bool transparencytoggle; 
   double scales[3];
   double trans[3];
   double rots[3];
   bool colorflag;
   double colors[3];
}GEOMETRYINFO;*/


//this class is used to contain the current geom info from gui 
class GeometryDataBuffer
{
   public:
      GeometryDataBuffer();
      ~GeometryDataBuffer();

   void SetCurrentGeomIndex(int);
   int GetCurrentGeomIndex();
   void SetCurrentModuleName(std::string);
   std::string GetCurrentModuleName();
   void SetCurrentModuleID(int);
   int GetCurrentModuleID();
   void ClearGeomInfoList();


   void SetCurrentGeomInfo_geomname(std::string geomname);
   void SetCurrentGeomInfo_modeltype(int type);
   void SetCurrentGeomInfo_geomfilename(std::string filename);
   void SetCurrentGeomInfo_transparencytoggle(bool toggle);
   void SetCurrentGeomInfo_scales(double scales[3]);
   void SetCurrentGeomInfo_trans(double trans[3]);
   void SetCurrentGeomInfo_rots(double rots[3]);
   void SetCurrentGeomInfo_colors(double colors[3]);
   void SetCurrentColorflag(bool flag);
   void SetCurrentLOD(double lod);
  
   void InitializeNewGeomInfo(int);
   void AddGeomInfo();
   void DeleteGeomInfo(int);
   void SetCurrentGeomInfoList(std::vector<GeometryInfoPackage>);
   std::vector <GeometryInfoPackage> GetCurrentGeomInfoList();
   std::vector <GeometryInfoPackage> GetCurrentModuleGeomInfoListFromMap();


   void UpdateCurrentGeomInfoListToMap();

   std::map <std::string, std::vector<GeometryInfoPackage> > GetWholeGeomInfoMap();

   private:

   std::vector <GeometryInfoPackage> _geominfopackagelist;

   std::map<std::string, std::vector<GeometryInfoPackage> > _geominfopackageVSmodule;
   int activedgeominfo;
   std::string cur_modulename;
   int cur_moduleid;
   
};
#endif
