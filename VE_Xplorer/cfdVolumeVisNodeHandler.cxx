#ifdef _OSG

#include <osg/Geometry>
#include <osg/Geode>
#include <osg/Group>
#include <osg/Switch>
#include <iostream>
#include "cfdVolumeVisNodeHandler.h"
//////////////////////////////////////////////////
//Constructors                                  //
//////////////////////////////////////////////////
cfdVolumeVisNodeHandler::cfdVolumeVisNodeHandler()
{
}
//////////////////////////////////////////////////////
cfdVolumeVisNodeHandler::cfdVolumeVisNodeHandler(const
                          cfdVolumeVisNodeHandler& vvnh)
{
   _bbox = vvnh._bbox;
   _bboxSwitch = new osg::Switch(*vvnh._bboxSwitch);
   _vvN = new osg::Group(*vvnh._vvN);
   _topNode = new osg::Group(*vvnh._topNode);
}
///////////////////////////////////////////////////
//Destructor                                     //
///////////////////////////////////////////////////
cfdVolumeVisNodeHandler::~cfdVolumeVisNodeHandler()
{
}
////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetBoundingBox(float* bbox)
{
   float minBBox[3];
   float maxBBox[3];
   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   minBBox[0] = bbox[0]; 
   minBBox[1] = bbox[2]; 
   minBBox[2] = bbox[4]; 
   maxBBox[0] = bbox[1]; 
   maxBBox[1] = bbox[3]; 
   maxBBox[2] = bbox[5]; 
   _bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));
  
}
///////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetVolumeVizNode(osg::Group* vvn)
{
   _vvN = vvn;
}
///////////////////////////////////////////////////////
osg::Group* cfdVolumeVisNodeHandler::GetVisualization()
{
   if(_topNode.valid()){
      return _topNode.get();
   } 
   return 0;
}
////////////////////////////////////
void cfdVolumeVisNodeHandler::Init()
{
   if(!_bbox.valid()){
      std::cout<<"Invalid bounding box!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_vvN.valid()){
      std::cout<<"Invalid volume viz node!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_topNode.valid()){
      _createVisualBBox();
      _topNode = new osg::Group();
      _topNode->setName("VolumeVisNodeHandler");

      //be able to turn the bounding box off/on
      _bboxSwitch = new osg::Switch();
      _bboxSwitch->setName("BBox Switch");
      _bboxSwitch->addChild(_visualBoundingBox.get());
      _topNode->addChild(_bboxSwitch.get());
      _attachVolumeVisNodeToGraph();
   }
}
///////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::_attachVolumeVisNodeToGraph()
{
   if(_bboxSwitch.valid()&&
      _visualBoundingBox.valid()&&
      _vvN.valid()){

      _visualBoundingBox->addChild(_vvN.get());
      _bboxSwitch->addChild(_vvN.get());
      _bboxSwitch->setSingleChildOn(0);
   }
}
///////////////////////////////////////////
void cfdVolumeVisNodeHandler::TurnOnBBox()
{
   if(_bboxSwitch.valid()){
      _bboxSwitch->setSingleChildOn(0);
   } 
}
///////////////////////////////////////////
void cfdVolumeVisNodeHandler::TurnOffBBox()
{
   if(_bboxSwitch.valid()){
      _bboxSwitch->setSingleChildOn(1);
   } 
}
/////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::_createVisualBBox()
{
   if(_bbox.valid()){
      _visualBoundingBox = new osg::Group();
      _visualBoundingBox->setName("VisHandler Bounding Box");

      osg::ref_ptr<osg::StateSet> ss = _visualBoundingBox->getOrCreateStateSet();
      ss->setRenderingHint(osg::StateSet::OPAQUE_BIN);
      ss->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
      
      osg::Geometry* bboxCube = new osg::Geometry;

      osg::Vec3Array* coords = new osg::Vec3Array();
      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(7));

      bboxCube->setVertexArray(coords);

      osg::Vec4Array* colors = new osg::Vec4Array(1);
      (*colors)[0].set(1.0f,1.0f,0.0f,1.0f);
      bboxCube->setColorArray(colors);
      bboxCube->setColorBinding(osg::Geometry::BIND_OVERALL);
      bboxCube->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES,
			       0,coords->size()));
      
      osg::Geode* geode = new osg::Geode;
      geode->addDrawable(bboxCube);
      geode->setName("Visual BBox");
      _visualBoundingBox->addChild(geode);
      
   }else{
      std::cout<<"Invalid bbox in cfdVolumeVisNodeHandler::_createVisualBBox!"<<std::endl;
   }
}

///////////////////////////////////////////////////////////////////////
//equal operator                                                     //
///////////////////////////////////////////////////////////////////////
cfdVolumeVisNodeHandler&
cfdVolumeVisNodeHandler::operator=(const cfdVolumeVisNodeHandler& vvnh)
{
   if(this != &vvnh){
      _bbox = vvnh._bbox;
      _vvN = vvnh._vvN;
      _topNode = vvnh._topNode;
      _visualBoundingBox = vvnh._visualBoundingBox;
      _bboxSwitch = vvnh._bboxSwitch;
   }
   return *this;
}

#endif //_OSG
