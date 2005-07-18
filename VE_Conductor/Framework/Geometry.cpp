#include "VE_Conductor/Framework/Geometry.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Conductor/Framework/interface.h"

#include <iostream>
#include <string>
#include <cmath>


Geometry::Geometry(std::string modulename)
{
     this->cur_modulename = modulename;
}

void Geometry::SetID(int id)
{
   mod_pack._id =id;
}


void Geometry::UnPack( Interface* intf )
{

   std::vector<std::string> vars;

   std::vector<GeometryInfoPackage> templist;

   GeometryInfoPackage temppackage;
   
   unsigned int i;
   long temp;

   mod_pack = *intf;
   vars = mod_pack.getGeomInfoPackages();
   
   bool rv =true;
   for(i=0;i<vars.size();i++)
   {
      temppackage = mod_pack.getGeomInfoPackage(vars[i], &rv);
      templist.push_back(temppackage);
      
   }

   GeometryDataManager::getInstance().GetGeometryDataBuffer()->SetCurrentGeomInfoList(templist);
   GeometryDataManager::getInstance().GetGeometryDataBuffer()->UpdateCurrentGeomInfoListToMap();
   
}

Interface* Geometry::Pack( void )
{
     
   SetCurModuleGeomInfoPackage();

   mod_pack._type = 2; //Module
   mod_pack._category = 1; // normal modules
   mod_pack._id = cur_moduleid;

   std::map<std::string, GeometryInfoPackage>::iterator iter;
   
   for(iter=_geominfopackagemap.begin();iter!=_geominfopackagemap.end();iter++)
   {
      mod_pack.setGeomInfoPackage(iter->first, iter->second);

   }

   return &mod_pack;
}

void Geometry::SetCurModuleGeomInfoPackage()
{
   std::map<std::string, std::vector<GeometryInfoPackage> > localmap =GeometryDataManager::getInstance().GetGeometryDataBuffer()->GetWholeGeomInfoMap();
   
   _geominfopackagemap.clear();

   std::vector<GeometryInfoPackage> locallist = localmap.find(cur_modulename)->second;
   
   for(unsigned int i=0; i<locallist.size(); i++)
   {
      _geominfopackagemap.insert(std::make_pair(locallist[i].GetGeomName(), locallist[i]));

   }

   this->cur_moduleid = GeometryDataManager::getInstance().GetGeometryDataBuffer()->GetCurrentModuleID();

   
}


