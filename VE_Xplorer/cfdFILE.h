/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: cfdFILE.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_FILE_H
#define CFD_FILE_H

#include <vector>
#include <string>

namespace VE_Xplorer
{
   class fileInfo;
}

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdNode;
}
#ifdef _PERFORMER
class pfFog;
#elif _OSG
namespace osg { class Fog; }
#endif

// Need to fix this class
// BIG PROBLEMS here
//class pfNode;
//class pfMaterial;
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdFILE 
{
public:
   cfdFILE( fileInfo*, VE_SceneGraph::cfdDCS* );
   cfdFILE( std::string, VE_SceneGraph::cfdDCS* );
   cfdFILE( float, float [ 3 ], std::string );

   ~cfdFILE();

   void Initialize(float);

   //void pfTravNodeMaterial(pfNode*, pfMaterial*, int );

   VE_SceneGraph::cfdDCS* GetDCS( void );
   VE_SceneGraph::cfdNode* GetNode( void );

   void SetFILEProperties( int, int, float* );
   int GetTransparentFlag( void );
   void setOpac(float op_val);
   float getOpacity();
   void setFog(double dist);

   std::string GetFilename( void );
   //pfLightModel *matLight;
   //pfMaterial *fmaterial;
   //pfMaterial *bmaterial;
   //std::vector< pfMaterial *> matList;
   VE_SceneGraph::cfdNode* node;
   VE_SceneGraph::cfdDCS*  DCS;
   //pfMaterial *mat1, *mat0;
   int mat_count;
   int color;
   int transparent;
   float stlColor [ 3 ];
   //char* fileName;
   std::string fileName;//[512];

   void SetRGBAColorArray( double* );
   void GetColorArray( void );
   void SetGeometryFilename( std::string );
   void SetModuleName( std::string );
   void SetTransparencyFlag( bool );
   void SetColorFlag( int );
   int GetColorFlag( void );
   std::string GetModuleName( void );
   void SetColorOfGeometry( VE_SceneGraph::cfdNode* );
   void Update( void );
   void SetOpacity( float );

private:
   float op;
#ifdef _PERFORMER
   pfFog* fog;
#elif _OSG
    osg::Fog* fog;
#endif
   double _rgba[ 4 ];
   bool _transparencyFlag;
   float _opacityLevel;
   int _colorFlag;
   VE_SceneGraph::cfdNode* _node;
   //cfdGroup* _masterNode;
   std::string _filename;
   std::string _moduleName;
};
}
#endif
