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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_TEXTURE_DATA_SET_H
#define CFD_TEXTURE_DATA_SET_H
#ifdef _OSG
#ifdef VE_PATENTED
namespace VE_TextureBased
{
   class cfdVolumeVisualization;
   class cfdTextureManager;
}
#include <vector>
#include <map>
#include <iostream>
#include <string>
#include "VE_Installer/include/VEConfig.h"
namespace VE_TextureBased
{
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
