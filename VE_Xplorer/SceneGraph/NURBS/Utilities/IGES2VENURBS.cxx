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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/NURBS/Utilities/IGES2VENURBS.h"
#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"

#include <Geom_BSplineSurface.hxx>
#include <TColStd_Array1OfReal.hxx>
#include <TColStd_Array1OfInteger.hxx>
#include <TColStd_Array2OfReal.hxx>
#include <TColgp_Array2OfPnt.hxx>
#include <IGESControl_Controller.hxx>
#include <IGESControl_Reader.hxx>
#include <IGESData_IGESModel.hxx>
#include <IGESData_IGESEntity.hxx>

using namespace NURBS::Utilities;

////////////////////////////////////////
///Constructor                        //
////////////////////////////////////////
IGES2VENURBS::IGES2VENURBS()
{
   ;
}
/////////////////////////////////////////
///Destructor                          //
/////////////////////////////////////////
IGES2VENURBS::~IGES2VENURBS()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< NURBS::NURBSSurface* > IGES2VENURBS::GetVectorOfVENURBSSurface( std::string igesFileName )
{
   // Read the file
   IGESControl_Controller::Init();
   IGESControl_Reader reader;
   reader.ReadFile( igesFileName.c_str() );
   Handle_IGESData_IGESModel model = reader.IGESModel();
   int nbEntities = model->NbEntities();
   for ( size_t i = 0; i < nbEntities; ++i )
   {
      Handle_IGESData_IGESEntity igesData = model->Entity( i );
      std::cout << "Type NB = " << igesData->TypeNumber() << " Form NB = " << igesData->FormNumber() << std::endl;
   }
//IGESConvGeom
   /*bsplineSurface = IGESToBRep_BasicSurface.TransferBSplineSurface();
   IGESToBRep.IsBasicSurface();
*/
   //pass the surfaces off to converter
   std::vector< NURBS::NURBSSurface* > surfaceVector;
   /*OCCNURBS2VENURBS occConverter;
   for ( size_t i = 0; i < numberOfSurfaces; ++i )
   {
      NURBS::NURBSSurface* temp = occConverter.GetVENURBSSurface( surface );
      surfaceVector.push_back( temp );
   }*/
   return surfaceVector;
}
