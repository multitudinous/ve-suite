#ifndef JPG_VARIANT_H_
#define JPG_VARIANT_H_

#include <loki/Typelist.h>
#include <loki/TypeManip.h>
#include <loki/Visitor.h>

#include <cstddef>
#include <typeinfo>

namespace Loki 
{
   /**
    * A Variant implementation based upon 
    * "An Implementation of Discriminated Unions in C++"
    * by Andrei Alexandrescu.  This three-part series was published in
    * April, June, and August of 2002 in the _C/C++ Users Journal_ "Experts
    * Forum"  Links to these articles can be found at http://erdani.org
    */

   /**
    * Computes the maximum size of a type in a typelist at compile time.
    */
   template <class TList>
   struct MaxSize;

   template <>
   struct MaxSize<Loki::NullType>
   {
      enum { result = 0 };  
   };

   /**
    * Computes the maximum size of a type in a typelist at compile time.
    */
   template <class H, class T>
   struct MaxSize< Loki::Typelist<H, T> >
   {
   private:
      enum { tailResult = size_t(MaxSize<T>::result) };
   public:
      enum { result = sizeof(H) > tailResult ? 
             sizeof(H) : size_t(tailResult) };
   };

   /**
    * Configurable Union
    * Takes a typelist and implements a C-style union containing the 
    * types defined in the typelist.
    */
   template <class TList> union ConfigurableUnion;

   template <class H, class T>
   union ConfigurableUnion< Loki::Typelist<H, T> >
   {
      H head_;
      ConfigurableUnion<T> tail_;
   };

   template <class H>
   union ConfigurableUnion< Loki::Typelist<H, Loki::NullType> >
   {
      H head_;
   };

   /**
    * Structify
    * Takes a type and turns it into a struct containing that type.
    * This is for satisfying alignment issues on compilers that align
    * structs of primitive types differently than the primitive type itself.
    */
   template <typename U>
   struct Structify
   {
      U dummy_;
   };

   /// Unknown class; just used for aligning user defined types.
   class Unknown;

   typedef LOKI_TYPELIST_36( 
        char,      
        short int,
        int,
        long int,
        float,
        double,
        long double,
        char*,
        short int*,
        int*,
        long int*,
        float*,
        double*,
        long double*,
        void*,
        Unknown (*)(Unknown),
        Unknown* Unknown::*,
        Unknown (Unknown::*)(Unknown),
        Structify<char>,
        Structify<short int>,
        Structify<int>,
        Structify<long int>,
        Structify<float>,
        Structify<double>,
        Structify<long double>,
        Structify<char*>,
        Structify<short int*>,
        Structify<int*>,
        Structify<long int*>,
        Structify<float*>,
        Structify<double*>,
        Structify<long double*>,
        Structify<void*>,
        Structify<Unknown (*)(Unknown)>,
        Structify<Unknown* Unknown::*>,
        Structify<Unknown (Unknown::*)(Unknown)>
        ) 
        TypesOfAllAlignments;

   /**
    * Eliminates from the TypesOfAllAlignments list the types that are
    * larger than the specified size.
    */
   template <class TList, size_t size>
   struct ComputeAlignBound;

   template <size_t size>
   struct ComputeAlignBound<Loki::NullType, size>
   {
      typedef Loki::NullType Result;
   };

   template <class H, class T, size_t size>
   struct ComputeAlignBound< Loki::Typelist<H, T>, size>
   {
      typedef typename ComputeAlignBound<T, size>::Result TailResult;
      typedef typename Loki::Select<
         sizeof(H) <= size,
         Loki::Typelist<H, TailResult>,
         TailResult>::Result Result;
   };

   /**
    * Aligned POD
    * Aligns to the correct POD type from the given typelist.
    */
   template <typename TList>
   class AlignedPOD
   {
      enum { maxSize = MaxSize<TList>::result };
      typedef typename ComputeAlignBound<TypesOfAllAlignments, maxSize>::Result
         AlignTypes;
   public:
      typedef ConfigurableUnion<AlignTypes> Result;
   };

   /**
    * EnsureOccurence
    * Enforces for Variants that a type given is only a type contained in
    * a typelist.
    */
   template <class TList, typename T>
   struct EnsureOccurence
   {
      typedef typename EnsureOccurence<typename TList::Tail, T>::Result Result;
   };

   template <class T, class U>
   struct EnsureOccurence<Loki::Typelist<T, U>, T>
   {
      typedef T Result;
   };

   /**
    * MakeConst
    * Transforms a typelist into a typelist containing const versions of
    * the original typelist.
    */
   template <class TList> struct MakeConst;
   template <> struct MakeConst<Loki::NullType>
   {
      typedef Loki::NullType Result;
   };

   template <typename Head, class Tail>
   struct MakeConst< Loki::Typelist<Head, Tail> >
   {
      typedef typename MakeConst<Tail>::Result NewTail;
      typedef Loki::Typelist<const Head, NewTail> Result;
   };

   /**
    * A Variant implementation based upon 
    * "An Implementation of Discriminated Unions in C++"
    * by Andrei Alexandrescu.  This three-part series was published in
    * April, June, and August of 2002 in the _C/C++ Users Journal_ "Experts
    * Forum"  Links to these articles can be found at http://erdani.org
    */
   template <class TList, class Align = typename AlignedPOD<TList>::Result>
   class Variant
   {
   public:
      /// Embedded Visitor types for client use.
      typedef Loki::Visitor<TList, void> StrictVisitor;
      // FIXME:  Loki::NonStrictVisitor is no longer available.
      //typedef Loki::NonStrictVisitor<bool, TList> NonStrictVisitor;
      /// const Embedded Visitor for client use.
      typedef Loki::Visitor<typename MakeConst<TList>::Result, void>
         ConstStrictVisitor;
      // FIXME:  Loki::NonStrictVisitor is no longer available.
      /*typedef Loki::NonStrictVisitor<bool, typename MakeConst<TList>::Result>
         ConstNonStrictVisitor;*/
   private:
      enum { size = MaxSize<TList>::result };
      union
      {
         unsigned char buffer_[size];
         Align dummy_;
      };

      struct VTable
      {
         /* // FIXME:  Conversions not implemented, so this Ctor is 
          * unnecessary.
         template <class T>
         VTable(Loki::Type2Type<T> tt)
         {
            typeId_ = &VTableImpl<T>::TypeId;
            destroy_ = &VTableImpl<T>::Destroy;
            clone_ = &VTableImpl<T>::Clone;
            cloneTypeOnly_ = &VTableImpl<T>::CloneTypeOnly;
            // FIXME:  Swap not implemented.
            //swap_ = &VTableImpl<T>::Swap;
            accept_ = &VTableImpl<T>::Accept;
            acceptConst_ = &VTableImpl<T>::AcceptConst;

            // FIXME:  Converter not implemented.
            //Init(changeType_, tt, TList());
         }*/
         const std::type_info& (*typeId_)();
         void (*destroy_)(const Variant&);
         void (*clone_)(const Variant&, Variant&);
         void (*cloneTypeOnly_)(const Variant&, Variant&);
         // FIXME:  Swap not implemented.
         //void (*swap_)(void* lhs, void* rhs);
         // FIXME:  Converter not implemented.
         //bool (*changeType_[Loki::TL::Length<TList>::value])(Variant&);
         // Visitor functions
         void (*accept_)(Variant&, StrictVisitor&);
         void (*acceptConst_)(const Variant&, ConstStrictVisitor&);
         // FIXME:  Converter not implemented.
         /*
         template <class T, TList>
         void Init(bool (**pChangeType)(Variant&), 
                   Loki::Type2Type<T> tt, TList)
         {
            typedef typename TList::Head Head;
            typedef typename TList::Tail Tail;
            enum { canConvert = Loki::Conversion<Head, T>::exists != 0 };
            *pChangeType = &VTableImpl<T>::Converter<Head, canConvert>::Convert;
            Init(pChangeType + 1, tt, Tail());
         }

         template <class T>
         void Init(bool (**)(Variant&), Loki::Type2Type<T>, Loki::NullType)
         {
            // end recursion
         }
         */
      };
      VTable* vptr_;

      template <class T>
      struct VTableImpl
      {
         static const std::type_info& TypeId()
         {
            return typeid(T);
         }

         static void Destroy(const Variant& var)
         {
            const T& data = 
               *reinterpret_cast<const T*>(&var.buffer_[0]);
            data.~T();
         }

         static void Clone(const Variant& src, Variant& dest)
         {
            new(&dest.buffer_[0]) T(
               *reinterpret_cast<const T*>(&src.buffer_[0]));
            dest.vptr_ = src.vptr_;
         }

         static void CloneTypeOnly(const Variant& src, Variant& dest)
         {
            new(&dest.buffer_[0]) T();
            dest.vptr_ = src.vptr_;
         }

         static void Accept(Variant& var, StrictVisitor& visitor)
         {
            typedef typename StrictVisitor::ReturnType RType;
            Loki::Visitor<T, RType> &v = visitor;
            v.Visit(*reinterpret_cast<T*>(&var.buffer_[0]));
         }

         static void AcceptConst(const Variant& var, 
                                 ConstStrictVisitor& visitor)
         {
            typedef typename ConstStrictVisitor::ReturnType RType;
            Loki::Visitor<const T, RType> &v = visitor;
            v.Visit( *reinterpret_cast<const T*>(&var.buffer_[0]));
         }
      };

   public:
      
      /**
       * Default Ctor
       * Initializes buffer to the first type in the typelist and
       * sets up the fake vtable.
       */
      Variant()
      {
         typedef typename TList::Head T;
         new(&buffer_[0]) T();
         static VTable vtbl_ =
         {
            &VTableImpl<T>::TypeId,
            &VTableImpl<T>::Destroy,
            &VTableImpl<T>::Clone,
            &VTableImpl<T>::CloneTypeOnly,
            &VTableImpl<T>::Accept,
            &VTableImpl<T>::AcceptConst
         };
         vptr_ = &vtbl_;
      }

      /**
       * Ctor
       * Initializes the buffer with the given type if it is in this
       * classes typelist, and sets up the fake vtable.
       */
      template <class T>
      Variant(const T& val)
      {
         typedef typename EnsureOccurence<TList, T>::Result Test;
         new(&buffer_[0]) T(val);
         static VTable vtbl_ =
         {
            &VTableImpl<T>::TypeId,
            &VTableImpl<T>::Destroy,
            &VTableImpl<T>::Clone,
            &VTableImpl<T>::CloneTypeOnly,
            &VTableImpl<T>::Accept,
            &VTableImpl<T>::AcceptConst
         };
         vptr_ = &vtbl_;
      }

      /**
       * Copy Ctor
       */
      Variant(const Variant& rhs)
      {
         (rhs.vptr_->clone_)(rhs, *this);
      }

      ~Variant()
      {
         (vptr_->destroy_)(*this);
      }

      const std::type_info& TypeId() const
      {
         return (vptr_->typeId_)();
      }

      template <typename T> T* GetPtr()
      {
         return TypeId() == typeid(T)
            ? reinterpret_cast<T*>(&buffer_[0])
            : 0;
      }

      template <typename T> T* GetPtr() const
      {
         return TypeId() == typeid(T)
            ? reinterpret_cast<T*>(&buffer_[0])
            : 0;
      }

      // FIXME:  Get reference functions are not implemented.

      void Accept(StrictVisitor& visitor)
      {
         (vptr_->accept_)(*this, visitor);
      }

      void Accept(ConstStrictVisitor& visitor) const
      {
         (vptr_->acceptConst_)(*this, visitor);
      }

      // FIXME:  Variant-to-Variant Conversions are not implemented.
   };
}

#endif
