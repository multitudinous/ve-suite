/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/scenegraph/nurbs/NCurve.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/KnotVector.h>
#include <iostream>

using namespace ves::xplorer::scenegraph::nurbs;
////////////////////////
//Constructor         //
////////////////////////
NURBSCurve::NURBSCurve( unsigned int degree )
        : NURBSObject( ves::xplorer::scenegraph::nurbs::NURBSObject::Curve, degree )
{
    _needsRetessellation = true;
}
/////////////////////////////////////////////
//Copy constructor                         //
/////////////////////////////////////////////
NURBSCurve::NURBSCurve( const NURBSCurve& rhs )
{}
/////////////////////////
NURBSCurve::~NURBSCurve()
{}
////////////////////////////////////////////////////////
NURBSCurve& NURBSCurve::operator=( const NURBSCurve& rhs )
{
    if( this != &rhs )
    {
        NURBSObject::operator =( rhs );
    }
    return *this;
}
///////////////////////////////////////////////////////////////////
//void NURBSCurve::_interpolateWithinRange( double umin, double umax,
//                                          double vmin, double vmax )
void NURBSCurve::_interpolateWithinModifiedRange( )
{

    //This function assumes all the proper checks have been made
    //before entering!!!!!
    double uparam = 0;//umin;
    //double vparam = vmin;
    unsigned int uIndexMin = m_modifiedUBounds[0];//_findNearestParameterIndex( "U", umin );
    unsigned int uIndexMax = m_modifiedUBounds[1];//_findNearestParameterIndex( "U", umax );
    //std::cout<<"umin: "<<uIndexMin<<std::endl;
    //std::cout<<"umax: "<<uIndexMax<<std::endl;
    std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> curveInfo;

    for( unsigned int u = uIndexMin; u <= uIndexMax; u++ )
    {
        uparam = _parameterValues["U"][u];
        curveInfo = _calculatePointOnCurve( uparam, _currentSpan["U"].at(u) );
        for( size_t k = 0; k < curveInfo.size(); k++ )
        {
            _interpolatedPoints[k][u] = curveInfo.at( k );
        }
        m_uvParameters[u] = ( ves::xplorer::scenegraph::nurbs::Point(uparam, 0, 0 ) );
    }
}
//////////////////////////////
void NURBSCurve::Interpolate()
{

    if( !_needsRetessellation )
        return;
    if( !_controlPoints[0].size() )
    {
        std::cout << "No control points specified!!" << std::endl;
        std::cout << "NURBSCurve::Interpolate()" << std::endl;
        return;
    }

    if( !_knotVectors["U"].NumberOfKnots() )
    {
        std::cout << "No knots specified!!" << std::endl;
        std::cout << "NURBSCurve::Interpolate()" << std::endl;
        return;
    }
    //Check our curve condition
    //m + 1 = (n + 1) + (p + 1)
    SetDegree( static_cast<unsigned int>( _knotVectors["U"].NumberOfKnots() - _controlPoints[0].size() ) - 1 );

    _interpolatedPoints.clear();
    m_uvParameters.clear();

    _interpolationStepSize["U"] = 1.0 / ( _meshDimensions["U"] - 1 );
    double param = 0.0;
    std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> curveInfo;

    for( unsigned int i = 0; i < _meshDimensions["U"]; i++ )
    {
        _calculateBasisFunctionsAndDerivatives( param,i,"U" );
        curveInfo = _calculatePointOnCurve( param, _currentSpan["U"].at(i) );
        for( size_t k = 0; k < curveInfo.size(); k++ )
        {
            _interpolatedPoints[k].push_back( curveInfo.at( k ) );
        }
        m_uvParameters.push_back ( ves::xplorer::scenegraph::nurbs::Point( param, 0, 0 ) );
        param += _interpolationStepSize["U"];
        _parameterValues["U"][param] = i;
    }
}
/////////////////////////////////////////////////////////////////////////////////////
std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> NURBSCurve::_calculatePointOnCurve( double parameter,
        unsigned int span )
{
    std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> resutlingWeightedPoint;
    double invWeight = 1.0f;
    double ctrlPtWeight = 1.0;
    double resultPtWeight = 1.0;
    double cw[3] = {0, 0, 0};
    unsigned int udegree = _degree["U"];
    for( unsigned int k = 0; k < udegree; k++ )
    {
        cw[0] = 0.0;
        cw[1] = 0.0;
        cw[2] = 0.0;
        resultPtWeight = 0.0;
        for( unsigned int j = 0; j <= udegree; j++ )
        {
            cw[0] += ( _controlPoints[0][span - udegree +j].WeightedX()
                       * _derivativeBasisFunctions["U"][k][parameter].at( j ) );

            cw[1] += ( _controlPoints[0][span - udegree +j].WeightedY()
                       * _derivativeBasisFunctions["U"][k][parameter].at( j ) );

            cw[2] += ( _controlPoints[0][span - udegree +j].WeightedZ()
                       * _derivativeBasisFunctions["U"][k][parameter].at( j ) );

            resultPtWeight += _controlPoints[0][span - udegree +j].Weight()
                              * _derivativeBasisFunctions["U"][k][parameter].at( j );
        }
        invWeight = 1.0 / resultPtWeight;
        resutlingWeightedPoint.push_back( ControlPoint( cw[0]*invWeight,
                                                        cw[1]*invWeight,
                                                        cw[2]*invWeight,
                                                        resultPtWeight ) );
    }
    return resutlingWeightedPoint;
}
