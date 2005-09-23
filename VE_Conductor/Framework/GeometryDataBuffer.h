/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: GeometryDataBuffer.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef GEOMETRY_DATABUFFER_H
#define GEOMETRY_DATABUFFER_H
#include "VE_Installer/include/VEConfig.h"

#include <fstream>
#include <vector>
#include <map>
class VE_CONDUCTOR_EXPORTS GeometryInfoPackage
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

//this class is used to contain the current geom info from gui 
class VE_CONDUCTOR_EXPORTS GeometryDataBuffer
{
public:
   GeometryDataBuffer();
   ~GeometryDataBuffer();

   GeometryDataBuffer( const GeometryDataBuffer& input )
   {
      ;
   }

   GeometryDataBuffer& operator= ( const GeometryDataBuffer& input )
   {
      if ( this != &input )
      {
         //this->_geominfopackageVSmodule = input._geominfopackageVSmodule;
         this->_geominfopackagelist = input._geominfopackagelist;

         this->activedgeominfo = input.activedgeominfo;
         this->cur_modulename = input.cur_modulename; 
         this->cur_moduleid = input.cur_moduleid;
      }
      return *this;
   }

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
  
   GeometryInfoPackage GetDefaultNewGeomInfoPackage(int);
   void AddGeomInfoToCurrentList(GeometryInfoPackage);
   void DeleteGeomInfosFromCurrentList(std::vector<int>);
   void SetCurrentGeomInfoList(std::vector<GeometryInfoPackage>);
   std::vector <GeometryInfoPackage> GetCurrentGeomInfoList();
      
   void UpdateGeomInfoToCurrentList(GeometryInfoPackage, int);  

   private:

	std::vector <GeometryInfoPackage> _geominfopackagelist;
 
   int activedgeominfo;
   std::string cur_modulename;
   int cur_moduleid;
   
};
#endif
