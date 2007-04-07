#include "VE_Xplorer/TextureBased/Database.h"

/// The following is from the Loki::Singleton library.  This allows a windows
/// DLL to have a Singleton with exactly one instance.  This example is
/// take from the Singleton test case in the Loki source tree, and the macro
/// MUST be called from a source file (as opposed to a header file) to work
/// correctly.
typedef Loki::SingletonHolder<VE_TextureBased::Database_t> DatabaseH;
LOKI_SINGLETON_INSTANCE_DEFINITION(DatabaseH)

namespace VE_TextureBased
{
   template<>
   Database_t& Singleton<Database_t>::Instance()
   {
      return Loki::SingletonHolder<Database_t>::Instance();
   }
   // Windows requires special measures for Singletons and DLLs.
#ifdef _WIN32
   template class Singleton<Database_t>;
#endif
}
template class Loki::Singleton<VE_TextureBased::Database_t>;
