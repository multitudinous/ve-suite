#ifdef VE_PATENTED
#ifdef _OSG 
#ifdef CFD_USE_SHADERS
#include "cfdAdvectPropertyCallback.h"
#include <osgUtil/Cullvisitor>
//////////////////////////////////////////////////////////////////////////
//Constructor                                                           //
//////////////////////////////////////////////////////////////////////////
cfdAdvectPropertyCallback::cfdAdvectPropertyCallback(osg::Node* subgraph)
:_subgraph(subgraph)
{

}
///////////////////////////////////////////////////////
//Destructor                                         //
///////////////////////////////////////////////////////
cfdAdvectPropertyCallback::~cfdAdvectPropertyCallback()
{
   
}

////////////////////////////////////////////////////////////////
void cfdAdvectPropertyCallback::operator()(osg::Node* node,
                                      osg::NodeVisitor* nv)
{
   if (nv && _subgraph.valid()){
      _subgraph->accept(*nv);   
   }
   // must call this             
   traverse(node,nv);
   
}
#endif
#endif
#endif
