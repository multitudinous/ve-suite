/* -*-c++-*- OpenSceneGraph - Copyright (C) 1998-2003 Robert Osfield 
 *
 * This library is open source and may be redistributed and/or modified under  
 * the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
 * (at your option) any later version.  The full license is in LICENSE file
 * included with this distribution, and on the openscenegraph.org website.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 * OpenSceneGraph Public License for more details.
*/
#include <osg/Geode>
#include <osg/StateSet>
#include <osg/CullFace>
#include <osg/Fade>

using namespace osg;

Fade::Fade() 
{
    _stateSet = new StateSet;
    _stateSet->setAttributeAndModes( new osg::Material );

    // Fix ME
    _stateSet->setMode( GL_BLEND, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );
    _stateSet->setRenderBinDetails(11,"RenderBin");
    _stateSet->setAttributeAndModes( new osg::CullFace  );
    _stateSet->setDataVariance( osg::Object::DYNAMIC );
    setStateSet( _stateSet.get());

    // Fade point 0.0 is implied
    _fadePoints[0.0] = 1.0;
}

Fade::Fade( const Fade &fade, const CopyOp & copyop ):
    Group(fade,copyop),
    _stateSet(fade._stateSet),
    _fadePoints(fade._fadePoints)
{
}

void Fade::addFadePoint( double distance, float fadeValue )
{
    // Clamp to 0.0 - 1.0
    if( fadeValue < 0.0 )
        fadeValue = 0.0;
    if( fadeValue > 1.0 )
        fadeValue = 1.0;

    _fadePoints[distance] = fadeValue;
}


bool Fade::addChild( Node *child )
{
    bool ret = osg::Group::addChild( child );
    _materials.clear();
    FindMaterials fm(_materials);
    accept( fm );

    std::vector<MaterialPair>::iterator p;
    for( p = _materials.begin(); p != _materials.end(); p++ )
    {
        p->material->setDataVariance(osg::Object::DYNAMIC);
        p->omaterial->setDataVariance(osg::Object::DYNAMIC);
    }

    return ret;
}

Fade::MaterialPair::MaterialPair( osg::Material *mat):
        material(mat)
{
    omaterial = new osg::Material((*material.get()));
}

void Fade::MaterialPair::setTransparency( float scale )
{
    osg::Vec4 v;
    if( material->getAmbientFrontAndBack() )
    {
        v = omaterial->getAmbient( osg::Material::FRONT_AND_BACK );
        v[3] *= scale;
        material->setAmbient( osg::Material::FRONT_AND_BACK, v );
    }
    else
    {
        v = omaterial->getAmbient( osg::Material::FRONT );
        v[3] *= scale;
        material->setAmbient( osg::Material::FRONT, v );

        v = omaterial->getAmbient( osg::Material::BACK );
        v[3] *= scale;
        material->setAmbient( osg::Material::BACK, v );
    }

    if( material->getSpecularFrontAndBack() )
    {
        v = omaterial->getSpecular( osg::Material::FRONT_AND_BACK );
        v[3] *= scale;
        material->setSpecular( osg::Material::FRONT_AND_BACK, v );
    }
    else
    {
        v = omaterial->getSpecular( osg::Material::FRONT );
        v[3] *= scale;
        material->setSpecular( osg::Material::FRONT, v );

        v = omaterial->getSpecular( osg::Material::BACK );
        v[3] *= scale;
        material->setSpecular( osg::Material::BACK, v );
    }

    if( material->getDiffuseFrontAndBack() )
    {
        v = omaterial->getDiffuse( osg::Material::FRONT_AND_BACK );
        v[3] *= scale;
        material->setDiffuse( osg::Material::FRONT_AND_BACK, v );
    }
    else
    {
        v = omaterial->getDiffuse( osg::Material::FRONT );
        v[3] *= scale;
        material->setDiffuse( osg::Material::FRONT, v );

        v = omaterial->getDiffuse( osg::Material::BACK );
        v[3] *= scale;
        material->setDiffuse( osg::Material::BACK, v );
    }

    if( material->getEmissionFrontAndBack() )
    {
        v = omaterial->getEmission( osg::Material::FRONT_AND_BACK );
        v[3] *= scale;
        material->setEmission( osg::Material::FRONT_AND_BACK, v );
    }
    else
    {
        v = omaterial->getEmission( osg::Material::FRONT );
        v[3] *= scale;
        material->setEmission( osg::Material::FRONT, v );

        v = omaterial->getEmission( osg::Material::BACK );
        v[3] *= scale;
        material->setEmission( osg::Material::BACK, v );
    }
}

Fade::FindMaterials::FindMaterials( std::vector<Fade::MaterialPair> &materials): 
            osg::NodeVisitor(TRAVERSE_ALL_CHILDREN),
            _materials(materials) {}

void Fade::FindMaterials::apply(osg::Group& node)
{ 
    processStateSet(node.getStateSet());
    traverse(node);
}

void Fade::FindMaterials::apply(osg::Geode& geode)
{
    for( unsigned int i = 0; i < geode.getNumDrawables(); i++ )
        processStateSet( geode.getDrawable(i)->getStateSet());
    traverse(geode);
}


void Fade::FindMaterials::processStateSet( osg::StateSet *sset )
{
    if( sset != 0L )
    {
        osg::Material *mat = dynamic_cast<osg::Material *>
            (sset->getAttribute( osg::StateAttribute::MATERIAL ));
        if( mat != 0L )
            _materials.push_back( MaterialPair(mat));
    }
}

void  Fade::_computeFade( float dist )
{
    float alpha = 0.0;

    if( dist < 0.0 )
        alpha = 0.0;
    else
    {
        std::map<double, float>::iterator p0;
        for( p0 = _fadePoints.begin(); p0 != _fadePoints.end(); p0++ )
        {
            std::map<double, float>::iterator p1 = p0;
            p1++;
            if( p1 == _fadePoints.end() )
            {
                alpha = p0->second;
                break;
            }
            else if( dist >= p0->first && dist < p1->first )
            {
                double mu = (dist - p0->first)/(p1->first - p0->first);
                alpha = (p0->second * (1.0 - mu)) + (p1->second * mu); 
                break;
            }
        }
    }

    if( alpha > 0.0 && alpha < 1.0 )
    {
        _stateSet->setMode( GL_BLEND, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );
        _stateSet->setMode( GL_DEPTH_TEST, osg::StateAttribute::OFF | osg::StateAttribute::OVERRIDE );
    }

    std::vector<MaterialPair>::iterator p;
    for( p = _materials.begin(); p != _materials.end(); p++ )
        p->setTransparency(alpha);
}


void Fade::traverse(NodeVisitor &nv)
{
    switch(nv.getTraversalMode())
    {
        case NodeVisitor::TRAVERSE_ALL_CHILDREN:
            std::for_each(_children.begin(),_children.end(),NodeAcceptOp(nv));
            break;

        case NodeVisitor::TRAVERSE_ACTIVE_CHILDREN:
            _computeFade( nv.getDistanceToEyePoint( getBound().center(), true ));
            std::for_each(_children.begin(),_children.end(),NodeAcceptOp(nv));
            break;

        default:
            break;
    }
}
