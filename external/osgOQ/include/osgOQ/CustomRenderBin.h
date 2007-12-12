//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#ifndef OSGOQ_CUSTOM_RENDER_BIN_H
#define OSGOQ_CUSTOM_RENDER_BIN_H 1

#include <osgUtil/RenderBin>

namespace osgOQ {

class CustomRenderBin : public osgUtil::RenderBin
{
public:
	CustomRenderBin();
    CustomRenderBin( const CustomRenderBin& rhs, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );

    virtual osg::Object* cloneType() const { return new CustomRenderBin(); }
    virtual osg::Object* clone(const osg::CopyOp& copyop) const { return new CustomRenderBin(*this,copyop); }
    virtual bool isSameKindAs(const osg::Object* obj) const { return dynamic_cast<const CustomRenderBin*>(obj)!=0L; }
    virtual const char* libraryName() const { return "osgOQ"; }
    virtual const char* className() const { return "CustomRenderBin"; }

	virtual void sortImplementation();

protected:
	virtual ~CustomRenderBin();
};

}


#endif
