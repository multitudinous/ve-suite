/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
#include "cfdGeomDataSet.h"

#include "cfdFileInfo.h"  

#include <assert.h>

#include <Performer/pfdu.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pr/pfGeoSet.h>
#include <Performer/pr/pfMaterial.h>
#include <Performer/pr/pfLight.h>
#include <Performer/pr.h>

#include <vtkGeometryFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkSTLReader.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vpr/Util/Debug.h>

cfdGeomDataSet::cfdGeomDataSet( fileInfo *geomFile, pfDCS *modelDCS  )
{
  mat0 = new pfMaterial();
  mat1 = new pfMaterial();
  DCS  = new pfDCS;

  vprDEBUG(vprDBG_ALL,1) << " File:1 " << geomFile->fileName
                         << std::endl << vprDEBUG_FLUSH;

  node = pfdLoadFile( geomFile->fileName );  // pfNode
  //node->ref();
  node->flatten( 0 );
  DCS->addChild( node );
  modelDCS->addChild( DCS );
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

cfdGeomDataSet::cfdGeomDataSet( float opVal, float stlColor[3], char *filename  )
{
   node = pfdLoadFile( filename );  // pfNode
   //node->ref();
   if ( stlColor[ 0 ] == -1 && stlColor[ 1 ] == -1 && stlColor[ 2 ] == -1 )
   {
      color = 0; 
   }
   else
   {
      color = 1;
   }

   vprDEBUG(vprDBG_ALL,1) << color << std::endl << vprDEBUG_FLUSH;
    
   if( color )
   {
      this->stlColor = new float[3];
      for(int i=0;i<3;i++)
      {
         this->stlColor[i] = stlColor[i];
      }
      vprDEBUG(vprDBG_ALL,1) << "this->stlColor = " << this->stlColor[0] 
         << " : " << this->stlColor[1] << " : " << this->stlColor[2]
         << std::endl << vprDEBUG_FLUSH;
   }
   
   //if ( opVal != 1 )
   {
      Initialize ( opVal );
   }
}


cfdGeomDataSet::~cfdGeomDataSet( )
{
   pfDelete( mat0 );
   pfDelete( mat1 );
   pfDelete( DCS );
   pfDelete( node );
   //pfDelete( matLight );
   std::cout << "Destroy cfdGeomDataSet" << std::endl;
}


void cfdGeomDataSet::Initialize(float op_val )
{
   this->op = op_val;
   setOpac( op_val );
}


void cfdGeomDataSet::setTrans( float t[3] )
{
   this->SetTrans( t[0], t[1], t[2] );
   vprDEBUG(vprDBG_ALL,1) << "Trans x: " << t[0] << " y: "
      << t[1] << " z: " << t[2] << std::endl << vprDEBUG_FLUSH;
}


void cfdGeomDataSet::SetTrans( float x, float y, float z )
{
   this->DCS->setTrans( x, y, z );
   vprDEBUG(vprDBG_ALL,1) << "Trans x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}

void cfdGeomDataSet::setScl( float x, float y, float z )
{
   this->DCS->setScale( x, y, z );
   vprDEBUG(vprDBG_ALL,1) << "Scale x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}

void cfdGeomDataSet::setRot(float h, float p, float r)
{
   this->DCS->setRot(h,p,r);
   vprDEBUG(vprDBG_ALL,1) << "Rot h: " << h << " p: " 
      << p << " r: " << r << std::endl << vprDEBUG_FLUSH;
}

void cfdGeomDataSet::setRotMat(double *rotate)
{
}

pfNode *cfdGeomDataSet::getpfNode( )
{
   return this->node;
}

pfDCS *cfdGeomDataSet::getpfDCS( )
{
   return this->DCS;
}

float cfdGeomDataSet::getOpacity()
{
   return this->op;
}


void cfdGeomDataSet::setOpac(float op_val)
{
   this->op = op_val;
}


