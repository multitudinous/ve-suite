#include "VE_Conductor/Framework/GeometryDataBuffer.h"
#include <iostream> 
#include <string>
#include <sstream>


GeometryInfoPackage::GeometryInfoPackage()
{
   this->modeltype=9;
   this->geomname ="default";
   this->geomfilename="undefined";
   this->transparencytoggle =1;
   this->scales[0]=1.0; 
   this->scales[1]=1.0; 
   this->scales[2]=1.0;
   this->trans[0]=0.0; 
   this->trans[1]= 0.0; 
   this->trans[2]=0.0;
   this->rots[0]=0.0;
   this->rots[1]=0.0;
   this->rots[2]=0.0;
   this->colorflag =1;
   this->colors[0]=1.0; 
   this->colors[1]=0.0; 
   this->colors[2]=0.0;
   this->LOD =0.5;
}

GeometryInfoPackage::~GeometryInfoPackage()
{

}

void GeometryInfoPackage::SetGeomName(std::string geomname)
{
   this->geomname = geomname;
}

void GeometryInfoPackage::SetModelType(int type)
{
   this->modeltype =type;
}

void GeometryInfoPackage::SetGeomFileName(std::string filename)
{
   this->geomfilename =filename;
}

void GeometryInfoPackage::SetTransparencyToggle(bool toggle)
{
   this->transparencytoggle=toggle;
}

void GeometryInfoPackage::SetScales(double x,double y,double z) 
{
   this->scales[0]= x;this->scales[1]=y;this->scales[2]=z;
}
      
void GeometryInfoPackage::SetRots(double x,double y,double z)
{
   this->rots[0]=x; this->rots[1]=y; this->rots[2]=z;
}

void GeometryInfoPackage::SetTrans(double x,double y,double z) 
{
   this->trans[0]=x; this->trans[1]= y; this->trans[2] =z;
}

void GeometryInfoPackage::SetColorFlag(bool flag)
{
   this->colorflag =flag;
}

void GeometryInfoPackage::SetColors(double r,double b,double g) 
{
   this->colors[0]=r; this->colors[1]=b; this->colors[2]=g;
}

void GeometryInfoPackage::SetLOD(double lod)
{
   this->LOD = lod;
}

std::string GeometryInfoPackage::GetGeomName()
{
   return this->geomname;
}

int GeometryInfoPackage::GetModelType()
{
   return this->modeltype;
}

std::string GeometryInfoPackage::GetGeomFileName()
{
   return this->geomfilename;
}

bool GeometryInfoPackage::GetTransparencyToggle()
{
   return this->transparencytoggle;
}

double* GeometryInfoPackage::GetScales()
{
   double* temp;
   temp = new double[3];
   for(int i=0; i<3;i++)
   {  
      temp[i] = this->scales[i];
   }
   return temp;
}


double* GeometryInfoPackage::GetTrans()
{
   double* temp;
   temp = new double[3];
   for(int i=0; i<3;i++)
   {  
      temp[i] = this->trans[i];
   }
   return temp;
}


double* GeometryInfoPackage::GetRots()
{
   double* temp;
   temp = new double[3];
   for(int i=0; i<3;i++)
   {  
      temp[i] = this->rots[i];
   }
   return temp;
}

double* GeometryInfoPackage::GetColors()
{
   double* temp;
   temp = new double[3];
   for(int i=0; i<3;i++)
   {  
      temp[i] = this->colors[i];
   }
   return temp;
}

bool GeometryInfoPackage::GetColorFlag()
{
   return this->colorflag;
}

double GeometryInfoPackage::GetLOD()
{
   return this->LOD;
}






GeometryDataBuffer::GeometryDataBuffer()
{
}

GeometryDataBuffer::~GeometryDataBuffer()
{
   
}

GeometryInfoPackage GeometryDataBuffer::GetDefaultNewGeomInfoPackage(int index)
{
   //give the new geominfopackage a default name
   const char base[] = "geom";
   //char geominfoname [80];
   //sprintf(geominfoname, "%s%d", base, index);
   std::ostringstream dirStringStream;
   dirStringStream << base << index;
   //std::string dirString = dirStringStream.str();
   //geominfoname = (char*)dirString.c_str();

   GeometryInfoPackage temppackage;

   temppackage.SetGeomName(dirStringStream.str());
   temppackage.SetGeomFileName("");
   temppackage.SetModelType(9);
   temppackage.SetTransparencyToggle(true);
   temppackage.SetScales(1.0, 1.0, 1.0);
   temppackage.SetTrans(0.0,0.0,0.0);
   temppackage.SetRots(0.0, 0.0, 0.0);
   temppackage.SetColorFlag(true);
   temppackage.SetColors(1.0, 0.0, 0.0);
   temppackage.SetLOD(0.5);
   
   //this->_geominfopackagelist.push_back(temppackage);

   return temppackage;
}



void GeometryDataBuffer::SetCurrentModuleName(std::string modulename)
{
   this->cur_modulename = modulename;
}

std::string GeometryDataBuffer::GetCurrentModuleName()
{
   return this->cur_modulename;
}

void GeometryDataBuffer::SetCurrentModuleID(int id)
{
   this->cur_moduleid=id;
}
int GeometryDataBuffer::GetCurrentModuleID()
{
   return this->cur_moduleid;
}
void GeometryDataBuffer::SetCurrentGeomIndex(int index)
{
   this->activedgeominfo =index;
}

void GeometryDataBuffer::ClearGeomInfoList()
{
   this->_geominfopackagelist.clear();
}

int GeometryDataBuffer::GetCurrentGeomIndex()
{
   return this->activedgeominfo;
}


void GeometryDataBuffer::SetCurrentGeomInfo_geomname(std::string geomname)
{
   _geominfopackagelist[this->activedgeominfo].SetGeomName (geomname);
}
void GeometryDataBuffer::SetCurrentGeomInfo_modeltype(int type)
{
   _geominfopackagelist[this->activedgeominfo].SetModelType(type);
}
   
void GeometryDataBuffer::SetCurrentGeomInfo_geomfilename(std::string filename)
{
   _geominfopackagelist[this->activedgeominfo].SetGeomFileName(filename);
}

void GeometryDataBuffer::SetCurrentGeomInfo_transparencytoggle(bool toggle)
{
   _geominfopackagelist[this->activedgeominfo].SetTransparencyToggle (toggle);
}

void GeometryDataBuffer::SetCurrentGeomInfo_scales(double scales[3])
{
   _geominfopackagelist[this->activedgeominfo].SetScales(scales[0], scales[1], scales[1]);
}

void GeometryDataBuffer::SetCurrentGeomInfo_trans(double trans[3])
{
   _geominfopackagelist[this->activedgeominfo].SetTrans(trans[0], trans[1], trans[2]);
}


void GeometryDataBuffer::SetCurrentGeomInfo_rots(double rots[3])
{
   _geominfopackagelist[this->activedgeominfo].SetRots(rots[0], rots[1], rots[2]);

}

void GeometryDataBuffer::SetCurrentGeomInfo_colors(double colors[3])
{
   _geominfopackagelist[this->activedgeominfo].SetColors(colors[0], colors[1], colors[2]);
}

void GeometryDataBuffer::SetCurrentColorflag(bool flag)
{
   _geominfopackagelist[this->activedgeominfo].SetColorFlag(flag);
}

void GeometryDataBuffer::SetCurrentLOD(double lod)
{
   _geominfopackagelist[this->activedgeominfo].SetLOD(lod);
}


void GeometryDataBuffer::AddGeomInfoToCurrentList(GeometryInfoPackage newgeominfopackage)
{
   _geominfopackagelist.push_back(newgeominfopackage);

}
 
void GeometryDataBuffer::DeleteGeomInfosFromCurrentList(std::vector<int> items)
{

   for(int i=0; i<items.size();i++)
   {
     _geominfopackagelist.erase(_geominfopackagelist.begin()+items[i]);      
   }

   if(_geominfopackagelist.size()==0)
   {
      std::cout<<"[DBG]... the curmodul's geominfo is clear"<<std::endl;
   }

   
  /*	for(itr = _geominfopackagelist.begin();itr!= _geominfopackagelist.end();itr++)
	{
		
      
      if(i==index)
		{
			_geominfopackagelist.erase(itr);
			break;
		}
		i++;
	}*/
   /*if(index>=0 && index<_geominfopackagelist.size())
   {
      _geominfopackagelist.erase();
   }*/
}

void GeometryDataBuffer::SetCurrentGeomInfoList(std::vector<GeometryInfoPackage> curlist)
{
   _geominfopackagelist.clear();
   this->_geominfopackagelist = curlist;
}

std::vector<GeometryInfoPackage> GeometryDataBuffer::GetCurrentGeomInfoList()
{
   return _geominfopackagelist;
}

void GeometryDataBuffer::UpdateGeomInfoToCurrentList(GeometryInfoPackage curpackage, int item)
{
   _geominfopackagelist[item]=curpackage;
   
}
void GeometryDataBuffer::UpdateCurrentGeomInfoListToMap()
{
   std::map<int, std::vector<GeometryInfoPackage> >::iterator itr =_geominfopackageVSmodule.find(cur_moduleid);
  
   
   if(itr!=_geominfopackageVSmodule.end()&& _geominfopackagelist.size()>0)
   {
      _geominfopackageVSmodule.erase(itr);
      _geominfopackageVSmodule.insert(std::make_pair(cur_moduleid, _geominfopackagelist));
   }
   else if(itr!=_geominfopackageVSmodule.end()&&_geominfopackagelist.size()==0)
   {
      _geominfopackageVSmodule.erase(itr);
   }
   else
   {
      _geominfopackageVSmodule.insert(std::make_pair(cur_moduleid, _geominfopackagelist));

   }
     
}

std::vector<GeometryInfoPackage> GeometryDataBuffer::GetCurrentModuleGeomInfoListFromMap()
{

   std::map<int, std::vector<GeometryInfoPackage> >::iterator itr=_geominfopackageVSmodule.find(cur_moduleid);

   if(itr!=_geominfopackageVSmodule.end())
   {
      _geominfopackagelist = itr->second;
      return _geominfopackagelist;
   }
   else
   {  
      //return a new package
      std::cout<<"[DBG]...can't find the geominfo match the cur module, so create a new one (default setting)"<<std::endl;
      const char base[] = "geom";
      //char geominfoname [80];
      //sprintf(geominfoname, "%s%d", base, 0);
      std::ostringstream dirStringStream;
      dirStringStream << base << 0;

      GeometryInfoPackage temppackage;

      std::vector<GeometryInfoPackage> templist;
      temppackage.SetModelType(9); 
      temppackage.SetGeomName(dirStringStream.str());
      temppackage.SetGeomFileName("");
      temppackage.SetTransparencyToggle(true);
      temppackage.SetScales(1.0, 1.0, 1.0);
      temppackage.SetTrans(0.0,0.0,0.0);
      temppackage.SetRots(0.0, 0.0, 0.0);
      temppackage.SetColorFlag(true);
      temppackage.SetColors(1.0, 0.0, 0.0);
      temppackage.SetLOD(0.5);
      
      _geominfopackagelist.clear();
      templist.push_back(temppackage);

      return templist;

   }

}
std::map<int, std::vector<GeometryInfoPackage> > GeometryDataBuffer::GetWholeGeomInfoMap()
{
   return _geominfopackageVSmodule;
}

