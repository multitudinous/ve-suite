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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef IGES_2_VE_NURBS_H
#define IGES_2_VE_NURBS_H
/*!\file IGES2VENURBS.h
  IGES2VENURBS API
  */
/*!\class IGES2VENURBS
 */
#include <ves/xplorer/scenegraph/NURBS/Utilities/OCCNURBS2VENURBS.h>
#include <ves/xplorer/scenegraph/NURBS/NSurface.h>
#include <ves/xplorer/scenegraph/NURBS/KnotVector.h>
#include <ves/xplorer/scenegraph/NURBS/ControlPoint.h>

#include <Geom_BSplineSurface.hxx>
#include <TColStd_Array1OfReal.hxx>
#include <TColStd_Array1OfInteger.hxx>
#include <TColStd_Array2OfReal.hxx>
#include <TColgp_Array2OfPnt.hxx>
#include <IGESControl_Controller.hxx>
#include <IGESControl_Reader.hxx>
#include <IGESData_IGESModel.hxx>
#include <IGESData_IGESEntity.hxx>
#include <IGESToBRep_BasicSurface.hxx>
#include <IGESGeom_BSplineSurface.hxx>
#include <GeomConvert.hxx>
#include <string>
#include <vector>

#include <ves/VEConfig.h>
namespace NURBS
{
namespace Utilities
{
///???
class VE_NURBS_UTILS_EXPORTS IGES2VENURBS
{
public:
   ///Constructor
   IGES2VENURBS(){ ; }
   ///Destructor
   ~IGES2VENURBS(){ ; }

   ///Get a filename for an IGES file and return a vector of VE NURBS Surfaces
   ///\param igesFileName IGES filename to be converted
   std::vector< NURBS::NURBSSurface* > GetVectorOfVENURBSSurface( std::string igesFileName )
   {
      std::vector< NURBS::NURBSSurface* > surfaceVector;
      OCCNURBS2VENURBS occConverter;
      IGESToBRep_BasicSurface surfaceConverter;
      // Read the file
      IGESControl_Controller::Init();
      IGESControl_Reader reader;   
      reader.ReadFile( const_cast< char* >( igesFileName.c_str() ) );
      Handle_IGESData_IGESModel model = reader.IGESModel();
      int nbEntities = model->NbEntities();
      for ( size_t i = 1; i <= nbEntities; ++i )
      {
         Handle_IGESData_IGESEntity igesData = model->Entity( i );
         std::cout << "Type NB = " << igesData->TypeNumber() << " Form NB = " << igesData->FormNumber() << std::endl;
         Handle_Geom_Surface tempGeomSurface = surfaceConverter.TransferBasicSurface( igesData );
         Handle_Geom_BSplineSurface tempPointer = GeomConvert::SurfaceToBSplineSurface( tempGeomSurface );
         //Handle_Geom_BSplineSurface tempPointer = surfaceConverter.TransferBSplineSurface( Handle(IGESGeom_BSplineSurface) igesData );
         //IGESConvGeom
         NURBS::NURBSSurface* temp = occConverter.GetVENURBSSurface( tempPointer );
         surfaceVector.push_back( temp );
      }
      /*bsplineSurface = IGESToBRep_BasicSurface.TransferBSplineSurface();
      IGESToBRep.IsBasicSurface();
      */
      return surfaceVector;
   }
};
}
}
#endif //IGES_2_VE_NURBS_H

