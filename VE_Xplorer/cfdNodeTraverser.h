#ifndef _CFD_NODE_TRAVERSER_H_
#define _CFD_NODE_TRAVERSER_H_
#include <Performer/pf/pfNode.h>
class cfdNodeTraverser{
public:
   cfdNodeTraverser();
   cfdNodeTraverser(const cfdNodeTraverser& cfdNT);
   ~cfdNodeTraverser();
   
   //the pre and post node callbacks
   typedef void (*preNodeTraverseCallback)(cfdNodeTraverser*,pfNode*);
   typedef void (*postNodeTraverseCallback)(cfdNodeTraverser*,pfNode*);

   //set the pre node traverse callback
   virtual void setPreNodeTraverseCallback(preNodeTraverseCallback func)
   {
      _preFunc = func;
   }
   //set the post node traverse callback
   virtual void setPostNodeTraverseCallback(postNodeTraverseCallback func)
   {
      _postFunc = func;
   }
   //set the node to traverse
   void setNode(pfNode* root);

   //begin traversing the node
   void traverse();

   //equal operator
   cfdNodeTraverser& operator=(const cfdNodeTraverser& cfdNT);
protected:
   //recurse the nodes
   virtual void _traverseNode(pfNode* currentNode);

   pfNode* _root;
   preNodeTraverseCallback _preFunc;
   postNodeTraverseCallback _postFunc;
};
#endif// _CFD_NODE_TRAVERSER_H_
