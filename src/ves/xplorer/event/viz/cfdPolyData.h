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
#ifndef CFD_POLYDATA_H
#define CFD_POLYDATA_H

#include <ves/xplorer/event/viz/cfdObjects.h>

class vtkPolyDataMapper;
class vtkWarpVector;

namespace ves
{
namespace xplorer
{
/*!\file cfdPolyData.h
cfdPolyData API
*/
/*!\class ves::xplorer::cfdPolyData
* 
*/
class VE_XPLORER_EXPORTS cfdPolyData : public cfdObjects
{
public:
   ///Constructor.
   ///\param op_val Set to 1.0.
   cfdPolyData( float op_val = 1.0 );

   ///Destructor.
   virtual ~cfdPolyData();

   ///Update.
   virtual void Update( void );

   ///In future, multi-threaded apps will make a copy of VjObs_i commandArray.
   virtual void UpdateCommand();

   ///Assigns particle option.
   ///\param option
   void SetParticleOption( unsigned int option);

   ///Gets particle option.
   unsigned int GetParticleOption();

   ///Sets particle scale.
   ///\param x
   void SetParticleScale( float x);

   ///Gets particle scale.
   float GetParticleScale();

private:
   float GetSphereScaleFactor();///Sphere scaling.

   std::string colorByScalar;///<String to hold color by scalar.

   vtkPolyDataMapper *map;///<Map for vtk.
   vtkWarpVector* warper;///<Warper for vtk.
   //cfdCommandArray* commandArray;
   bool warpSurface;///Test for warped surface.
   double warpedContourScale;///<warped contour scale value

   unsigned int  _particleOption;///<point cloud or variably sized spheres.
   float _particleScale;///<particle scale.
};
}
}
#endif
