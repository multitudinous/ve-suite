/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_READPARAM_H
#define CFD_READPARAM_H
/*!\file cfdReadParam.h
cfdReadParam API
*/
/*!\class VE_Xplorer::cfdReadParam
* 
*/
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

#include "VE_Xplorer/SceneGraph/DCS.h"

#include <vector>
#include <string>

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

class vtkDataSet;

namespace VE_Xplorer
{
   class cfdDataSet;
   class cfdCommandArray;
}

namespace VE_SceneGraph
{
	class DCS;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdReadParam : public cfdGlobalBase
{
public:
   cfdReadParam();
   ~cfdReadParam();

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   int   numGeoms;
   int   bmpFile;
   char  bmpFileName[ 100 ];
   char  quatCamFileName[ 100 ];
   double bmpPosition[ 3 ];
   int bmpOrientation;  // 0=X-plane, 1=Y-plane, and 2=Z-plane.
   float isoScale;
   float delta;
   char  textLine[ 256 ];
   int   *guiVal;
   float diameter;

   float scalarBarPos[ 3 ];
   float scalarBarZRot;
   float scalarBarH;
   float scalarBarW;

   //BY YANG for ANIMATED IMG
   char basename[256];
   int frames;
   int ex_x, ex_y;
   int dim;
   double origin[3];
   double spacing[3];

   //END of YANG

   // IHCC Model - should be deleted at a later date
   bool ihccModel;
   bool changeGeometry;

   void CreateNewDataSet();
   int GetNumberOfDataSets();
   VE_Xplorer::cfdDataSet * GetDataSet( int i );
   VE_Xplorer::cfdDataSet * GetDataSetWithName( const std::string );

   int  convertDecimalToBinary( double );
   void convertBinaryToDecimal( int );
   void convertBinaryToArray( int, int );

   std::string readDirName( std::ifstream &inFile, std::string description );
   int readID( std::ifstream &inFile );

   float worldScale[ 3 ];
   float worldTrans[ 3 ];
   float worldRot[ 3 ];

   float imageScale[ 3 ];
   float imageTrans[ 3 ];
   float imageRot[ 3 ];

	osg::ref_ptr< VE_SceneGraph::DCS > dashBoardDCS;
   std::string dashboardFilename;

private:
   std::vector< VE_Xplorer::cfdDataSet * > dataSets;
   std::vector< double > testBin;
   //void LoadSurfaceFiles( char * dir );
};
}
#endif
