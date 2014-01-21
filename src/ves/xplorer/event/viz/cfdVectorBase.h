/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CFD_VECTOR_BASE_H
#define CFD_VECTOR_BASE_H

#include <ves/xplorer/event/viz/cfdObjects.h>

class vtkGlyph3D;
class vtkPolyDataMapper;
class vtkMaskPoints;
class vtkTriangleFilter;
class vtkStripper;
class vtkThresholdPoints;

namespace ves
{
namespace xplorer
{
/*!\file cfdVectorBase.h
 * cfdVectorBase API
 * class ves::xplorer::cfdVectorBase
 *
 */
class VE_XPLORER_EXPORTS cfdVectorBase : public cfdObjects
{
public:
    ///Constructor
    cfdVectorBase();

    ///Copy Constructor.
    cfdVectorBase( cfdVectorBase const& src );

    ///Destructor
    virtual ~cfdVectorBase();

    // pure virtual int functions to be specified in concrete implementations

    /// in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand();

    ///Update the actor
    virtual void Update() = 0;

    ///Create a copy of this object
    virtual cfdObjects* CreateCopy() = 0;

    ///Set threshold
    ///\params minThresh and maxThresh, min and max percentage values for threshold
    void SetThreshHoldPercentages( int minThresh, int maxThresh );

    ///Set/Get threshold values
    ///\param ????
    void SetThreshHoldValues( double* );
    double* GetThreshHoldValues( void );

    ///Update function
    void UpdateThreshHoldValues();


    ///Set/Get vector ratio factor
    void SetVectorRatioFactor( int );
    int GetVectorRatioFactor();

    ///Set/Get scale by factor
    void SetScaleByVectorFlag( int );
    int GetScaleByVectorFlag( void );

    ///Set/Get vector scales
    void SetVectorScale( float );
    float GetVectorScale();

    ///Update the property set
    void UpdatePropertySet();

protected:
	void CreateLFXPlane();
    void CreateArbSurface();

    vtkGlyph3D* glyph;
    vtkPolyDataMapper*   mapper;
    vtkMaskPoints*       ptmask;
    vtkTriangleFilter*   tris;
    vtkStripper*         strip;
    vtkThresholdPoints*  tfilter;

    void SetGlyphWithThreshold();
    void SetGlyphAttributes();
    float GetVectorScaleFactor();

    int _vectorThreshHoldMinPercentage;
    int _vectorThreshHoldMaxPercentage;
    double _vectorThreshHoldValues[ 2 ];
    int _vectorRatioFactor;
    int _scaleByVector;
    float _vectorScale;
    unsigned int m_selectDataMapping;
};
}
}
#endif
