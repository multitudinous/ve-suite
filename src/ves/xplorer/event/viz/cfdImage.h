/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef CFD_IMAGE_H
#define CFD_IMAGE_H

#include <ves/xplorer/event/viz/cfdObjects.h>

class vtkBMPReader;
class vtkImageReader;
class vtkPlaneSource;
class vtkPolyDataMapper;
class vtkTexture;
class vtkActor;

namespace ves
{
namespace xplorer
{
/*!\file cfdImage.h
cfdImage API
*/
/*!\class ves::xplorer::cfdImage
*
*/
class VE_XPLORER_EXPORTS cfdImage : public cfdObjects
{
public:

    ///Reads in parameters for animated image
    ///\param param
    cfdImage( std::string param );

    ///Fix needed to add the new style read param to this class
    ///takes code out of cfdReadParam to this function
    ///\param filename
    ///\param ex_x
    ///\param ex_y
    ///\param dim
    ///\param origin
    ///\param spacing
    cfdImage( std::string filename, int ex_x, int ex_y, int dim,
              double *origin, double *spacing );

    ///Destructor.
    virtual ~cfdImage( );

    ///In future, multi-threaded apps will make a copy of VjObs_i commandArray.
    virtual void UpdateCommand();

    ///Update the actor/
    virtual void Update( void );

    ///Get actor.
    vtkActor* GetActor( void );

    ///Create objects for image.
    void CreateObjects( void );

private:
    int type;///<Direction: 0=X-plane, 1=Y-plane, and 2=Z-plane.
    char typeLabel;///<'X', 'Y', or 'Z'.

    vtkBMPReader *bmpReader;///<Bitmap reader.
    vtkImageReader *imgReader;///<Image reader.
    vtkPlaneSource *plane;///<Plane source for vtk.
    vtkPolyDataMapper *mapper;///<Polydata mapper for vtk.
    vtkTexture *texture;///<Texture for vtk.
    //char  bmpFileName[ 100 ];
    std::string bmpFileName;///<Stores bitmap filename.
    double bmpPosition[ 3 ];///<Stores bitmap position.
    int bmpOrientation;///<0=X-plane, 1=Y-plane, and 2=Z-plane.
    std::string _param;///<The string storing parameters.
};
}
}
#endif
