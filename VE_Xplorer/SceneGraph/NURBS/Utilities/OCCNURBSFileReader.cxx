/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *    - ISU's Thermal Systems Virtual Engineering Group,
 *      Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *    - Reaction Engineering International, www.reaction-eng.com
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
 * Version:         $Rev$
 * Author:          $Author$
 * Id:                $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/NURBS/Utilities/OCCNURBSFileReader.h"
#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include <fstream>
#include <iostream>
#include <sstream>
/*!\file OCCNURBSFileReader.cxx
 * OCCNURBSFileReader Code
 */
using namespace NURBS::Utilities;
////////////////////////////////////////////////////////////////////////////////
//Constructor                         //
////////////////////////////////////////
OCCNURBSFileReader::OCCNURBSFileReader()
{
    _surfacePatch = 0;
}
////////////////////////////////////////////////////////////////////////////////
//Destructor                           //
/////////////////////////////////////////
OCCNURBSFileReader::~OCCNURBSFileReader()
{
    if( _surfacePatch )
    {
        delete _surfacePatch;
    }
    _surfacePatch = 0;
}
////////////////////////////////////////////////////////////////////////////////
//Read in a NURBS patch created by the NURBSPointCreator utility
////////////////////////////////////////////////////////////////////////////////
NURBS::NURBSSurface* OCCNURBSFileReader::ReadPatchFile( std::string star2occFile )
{
    std::fstream occNURBSFile( star2occFile.c_str(),std::ios::in );
    if(occNURBSFile.is_open())
    {
        int fileLength = 0;
        occNURBSFile.seekg( 0, std::ios_base::end );
        fileLength = occNURBSFile.tellg();
        occNURBSFile.seekg( 0, std::ios_base::beg );

        char* descriptorLine = new char[ fileLength ];
        std::cout<<"Opened file: "<<star2occFile<<std::endl;
        
        //U knots descriptor
        occNURBSFile.getline( descriptorLine, fileLength, '\n' );

        //U knot values
        occNURBSFile.getline( descriptorLine, fileLength, '\n' );

        std::istringstream strm( descriptorLine );
        double knot = 0;
        NURBS::KnotVector uKnots;        
    
        while( strm >> knot )
        {
            uKnots.AddKnot( knot );
        }

        //V knots descriptor
        occNURBSFile.getline( descriptorLine, fileLength, '\n' );

        //V knot values
        occNURBSFile.getline( descriptorLine, fileLength, '\n' );
        strm.clear();
        strm.str( descriptorLine );
        NURBS::KnotVector vKnots;
        while( strm >> knot )
        {
            vKnots.AddKnot( knot );
        }
          
        std::vector< NURBS::ControlPoint > surfaceCtrlPts;
        double x = 0;
        double y = 0;
        double z = 0;
        double w = 0;
        //Control points descriptor
        occNURBSFile.getline( descriptorLine, fileLength, '\n' );

        //control points
        char delimChar[256];
        unsigned int nU = 0;
        unsigned int nV = 0;
        while( occNURBSFile.getline( descriptorLine, fileLength,'\n' ) )
        {
            nU = 0;
            strm.clear();
            strm.str( descriptorLine );
            while( strm >> delimChar )
            {
                strm>>x>>y>>z>>w;
                surfaceCtrlPts.push_back( NURBS::ControlPoint( x, y, z, w ) );
                nU++;
                //")"
                strm>>delimChar;
            }
            nV++;
        }

        occNURBSFile.close();
        delete [] descriptorLine;
        descriptorLine =0;
        //User responsible for deleting memory!!!
        NURBS::NURBSSurface* surfacePatch = new NURBS::NURBSSurface();
        surfacePatch->SetControlPoints( surfaceCtrlPts, nU, nV );
        surfacePatch->SetKnotVector( uKnots, "U" );
        surfacePatch->SetKnotVector( vKnots, "V" );

        return surfacePatch;
    }
    else
    {
        std::cout<<"Could not open file: "<<star2occFile<<std::endl;
        return 0;
    }
}

