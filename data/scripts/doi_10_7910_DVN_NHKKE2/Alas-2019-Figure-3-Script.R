#Figure 3 - Co-occurrence network (Biblioshiny)
ifelse(!require(bibliometrix),install.packages("bibliometrix", dependencies=TRUE),1)
require(bibliometrix)
biblioshiny()
# See field tags: http://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf