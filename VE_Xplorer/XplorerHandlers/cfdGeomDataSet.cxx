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
 * File:          $RCSfile: cfdGeomDataSet.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdGeomDataSet.h"

#include "VE_Xplorer/XplorerHandlers/cfdFileInfo.h"  

#include <cassert>
#include "VE_Xplorer/SceneGraph/cfdGeode.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"

//shouldn't have to declare these
//should instead use the cfdNode types!!!
#ifdef _PERFORMER
#include <Performer/pfdu.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pr.h>
#elif _OSG
#include <osg/Geode>
#include <osg/MatrixTransform>
#include <osg/Light>
#include <osg/Material>
#endif

#include <vtkGeometryFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkSTLReader.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

//cfdGeomDataSet::cfdGeomDataSet( fileInfo *geomFile, pfDCS *modelDCS  )
cfdGeomDataSet::cfdGeomDataSet( fileInfo *geomFile, VE_SceneGraph::cfdDCS *modelDCS  )
{
#ifdef _PERFORMER
  mat0 = new pfMaterial();
  mat1 = new pfMaterial();
#elif _OSG
   mat0 = new osg::Material();
   mat1 = new osg::Material();
#endif
  DCS  = new VE_SceneGraph::cfdDCS();

  vprDEBUG(vesDBG,1) << " File:1 " << geomFile->fileName
                         << std::endl << vprDEBUG_FLUSH;

  //node = pfdLoadFile( geomFile->fileName );  // pfNode
  //node->ref();
  //node->flatten( 0 );
  node->LoadFile(geomFile->fileName);

  DCS->AddChild( node );
  modelDCS->AddChild( DCS );
  mat_count =0;
  transparent = geomFile->trans;
  color = geomFile->color; 
    
   if( color )
   {
      stlColor = new float[3];
      for(int i=0;i<3;i++)
      {
         stlColor[i] = geomFile->stlColor[i];
      }
   }

}
//////////////////////////////////////////////////////////////////////////////////
cfdGeomDataSet::cfdGeomDataSet( float opVal, float stlColor[3], std::string filename  )
{
   //node = pfdLoadFile( filename );  // pfNode
   //node->ref();
   node->LoadFile(filename);
   if ( stlColor[ 0 ] == -1 && stlColor[ 1 ] == -1 && stlColor[ 2 ] == -1 )
   {
      color = 0; 
   }
   else
   {
      color = 1;
   }

   vprDEBUG(vesDBG,1) << color << std::endl << vprDEBUG_FLUSH;
    
   if( color )
   {
      this->stlColor = new float[3];
      for(int i=0;i<3;i++)
      {
         this->stlColor[i] = stlColor[i];
      }
      vprDEBUG(vesDBG,1) << "this->stlColor = " << this->stlColor[0] 
         << " : " << this->stlColor[1] << " : " << this->stlColor[2]
         << std::endl << vprDEBUG_FLUSH;
   }
   
   //if ( opVal != 1 )
   {
      Initialize ( opVal );
   }
}

//////////////////////////////////
cfdGeomDataSet::~cfdGeomDataSet( )
{
   delete DCS;
   delete node;
   /*
   pfDelete( mat0 );
   pfDelete( mat1 );
   pfDelete( DCS );
   pfDelete( node );*/
   //pfDelete( matLight );
   std::cout << "Destroy cfdGeomDataSet" << std::endl;
}

//////////////////////////////////////////////
void cfdGeomDataSet::Initialize(float op_val )
{
   this->op = op_val;
   setOpac( op_val );
}

///////////////////////////////////////////
void cfdGeomDataSet::setTrans( float t[3] )
{
   this->setTrans3( t[0], t[1], t[2] );
   vprDEBUG(vesDBG,1) << "Trans x: " << t[0] << " y: "
      << t[1] << " z: " << t[2] << std::endl << vprDEBUG_FLUSH;
}

////////////////////////////////////////////////////////////
void cfdGeomDataSet::setTrans3( float x, float y, float z )
{
   float trans[3];
   trans[0] = x;
   trans[1] = y;
   trans[2] = z;
   this->DCS->SetTranslationArray(trans);//SetTrans( x, y, z );
   vprDEBUG(vesDBG,1) << "Trans x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////
void cfdGeomDataSet::setScl( float x, float y, float z )
{
   float scale[3];
   scale[0] = x;
   scale[1] = y;
   scale[2] = z;
   
   this->DCS->SetScaleArray(scale);//>SetScale( x, y, z );
   vprDEBUG(vesDBG,1) << "Scale x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}
//////////////////////////////////////////////////////
void cfdGeomDataSet::setRot(float h, float p, float r)
{
   float rot[3];
   rot[0] = h;
   rot[1] = p;
   rot[2] = r;
   this->DCS->SetRotationArray(rot);//>SetRot(h,p,r);
   vprDEBUG(vesDBG,1) << "Rot h: " << h << " p: " 
      << p << " r: " << r << std::endl << vprDEBUG_FLUSH;
}
//////////////////////////////////////////////
void cfdGeomDataSet::setRotMat(double *rotate)
{
}
////////////////////////////////////
VE_SceneGraph::cfdNode *cfdGeomDataSet::getpfNode( )
{
   return this->node;
}
//////////////////////////////////
VE_SceneGraph::cfdDCS *cfdGeomDataSet::getpfDCS( )
{
   return this->DCS;
}
//////////////////////////////////
float cfdGeomDataSet::getOpacity()
{
   return this->op;
}

//////////////////////////////////////////
void cfdGeomDataSet::setOpac(float op_val)
{
   this->op = op_val;
}


