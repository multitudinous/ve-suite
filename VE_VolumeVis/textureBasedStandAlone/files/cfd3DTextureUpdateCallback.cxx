#include "cfd3DTextureUpdateCallback.h"
////////////////////////////////////////////////////////////////////////////
cfd3DTextureUpdateCallback::cfd3DTextureUpdateCallback(osg::Node* subgraph)
:_subgraph(subgraph) 
{
               
}
////////////////////////////////////////////////////////////
void cfd3DTextureUpdateCallback::operator()(osg::Node* node, 
                                       osg::NodeVisitor* nv)
{
   // traverse the subgraph to update any nodes.
   if (_subgraph.valid()){
      _subgraph->accept(*nv);
   }
   // must traverse the Node's subgraph            
   traverse(node,nv);

}
