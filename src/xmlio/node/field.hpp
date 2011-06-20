#ifndef __XMLIO_CField__
#define __XMLIO_CField__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"
#include "functor.hpp"
#include "functor_type.hpp"
#include "duration.hpp"
#include "date.hpp"
#include "declare_group.hpp"
#include "calendar_util.hpp"

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CFieldGroup;
   class CFieldAttributes;
   class CField;

   class CFile;
   class CGrid;

   ///--------------------------------------------------------------

   // Declare/Define CFieldAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CField)
#  include "field_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CField)

   ///--------------------------------------------------------------
   class CField
      : public CObjectTemplate<CField>
      , public CFieldAttributes
   {
         /// friend ///
         friend class CFile;

         /// typedef ///
         typedef CObjectTemplate<CField>   SuperClass;
         typedef CFieldAttributes SuperClassAttribute;

      public :

         typedef CFieldAttributes RelAttributes;
         typedef CFieldGroup      RelGroup;

         /// Constructeurs ///
         CField(void);
         explicit CField(const StdString & id);
         CField(const CField & field);       // Not implemented yet.
         CField(const CField * const field); // Not implemented yet.

         /// Accesseurs ///
               boost::shared_ptr<CField> getDirectFieldReference(void) const;
         const boost::shared_ptr<CField> getBaseFieldReference(void)   const;
         const std::vector<boost::shared_ptr<CField> > & getAllReference(void) const;

         boost::shared_ptr<CGrid> getRelGrid(void) const ;
         boost::shared_ptr<CFile> getRelFile(void) const ;

      public :

         const date::CDuration & getFreqOperation(void) const;
         const date::CDuration & getFreqWrite(void) const;

         boost::shared_ptr<date::CDate> getLastWriteDate(void) const;
         boost::shared_ptr<date::CDate> getLastOperationDate(void) const;

         boost::shared_ptr<func::CFunctor> getFieldOperation(void) const;
         
         ARRAY(double, 1) getData(void) const;

         const StdString & getBaseFieldId(void) const;

         /// Mutateur ///
         void setRelFile(const boost::shared_ptr<CFile> _file);

         template <StdSize N>
            inline bool updateData
               (const date::CDate & currDate, const ARRAY(double, N) data);

         bool updateDataServer
               (const date::CDate & currDate, const std::deque<ARRAY(double, 1)> storedClient);

      public :

         /// Test ///
         bool hasDirectFieldReference(void) const;

         /// Traitements ///
         void solveRefInheritance(void);
         void solveGridReference(void);
         void solveOperation(void);

         virtual void fromBinary(StdIStream & is);

         /// Destructeur ///
         virtual ~CField(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

      private :

         /// Propriétés privées ///
         
         std::vector<boost::shared_ptr<CField> > refObject;
         boost::shared_ptr<CField> baseRefObject;
         boost::shared_ptr<CGrid>  grid ;
         boost::shared_ptr<CFile>  file;

         date::CDuration freq_operation, freq_write;

         boost::shared_ptr<date::CDate>    last_Write, last_operation;
         boost::shared_ptr<func::CFunctor> foperation;
         
         ARRAY(double, 1) data;

   }; // class CField

   ///--------------------------------------------------------------

   // Declare/Define CFieldGroup and CFieldDefinition
   DECLARE_GROUP(CField);

   } // namespace tree

   ///-----------------------------------------------------------------

   template <>
      void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void);

   ///-----------------------------------------------------------------
} // namespace xmlioserver

#include "grid.hpp"

namespace xmlioserver {
namespace tree {

   template <StdSize N>
      bool CField::updateData(const date::CDate & currDate, const ARRAY(double, N) data)
   {
      if ((*last_operation + freq_operation) >= currDate)
      {
         ARRAY_CREATE(input, double, 1, [0]);
         input->resize(boost::extents[data->size()]);
         this->grid->inputField(data, input);
         (*this->foperation)(input);
         *last_operation = currDate;
      }
      
      if ((*last_Write + freq_write) >= currDate)
      {
         *last_Write = currDate;
         return (true);
      }
      return (false);
   };

} // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CField__
