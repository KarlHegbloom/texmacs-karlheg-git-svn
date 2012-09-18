<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|A <tt|tmfs> primer>

  <subsection|The <TeXmacs> filesystem>

  Many things in <TeXmacs> can be referenced through a <abbr|URI> with
  <tt|tmfs> as schema. Examples of entities in this system are buffers, views
  and windows or at a higher level help buffers and search results. A
  <TeXmacs> <abbr|URI> follows the format:

  <center|<tt|tmfs://handler[/query]>>

  Requests to open <abbr|URI>s such as these are sent to a <em|handler>,
  which actually is a set of procedures implementing the basic operations
  related to the type of content they handle: loading the content, saving it
  (if possible or necessary), setting the window title and establishing
  access permissions are the basic operations. Predefined handlers which the
  user usually encounters are <tt|grep>, <tt|help>, <tt|history>,
  <tt|revision> and <tt|sapi>: they accept a query representing search
  strings, files or help pages and render results in the appropriate language
  into a new buffer. The <em|query> is a string in the usual format
  <tt|variable1=value1&variable2=value2>. Its parsing can be done using
  <scm|query-ref>.

  Situations where using this system makes more sense than regular documents
  are for instance documentation, which must be chosen from several languages
  and possibly be compiled on the fly from various sources (see module
  <tt|<hlink|doc.sapi|tmfs://sapi/type=module&what=doc.sapi>>) and
  automatically generated content, like that resulting from interacting from
  an external system for version control of documents (see handler
  <tt|version> in module <tt|<hlink|version.version-tmfs|tmfs://sapi/type=module&what=version.version-tmfs>>).

  <subsection|Implementing a handler>

  The definition of a handler is done via <scm|tmfs-handler> or with the
  convenience macros <scm|tmfs-load-handler>, <scm|tmfs-save-handler>,
  <scm|tmfs-permission-handler> and <scm|tmfs-title-handler>.

  Below we'll implement a basic load handler named <tt|simple> which will
  accept two sorts of arguments: <scm|type> and <scm|what>. We shall use two
  procedures, one to handle the requests, another to create the document.

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tm-define (simple-load header body)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document (section ,header) ,body))))
    <|unfolded-io>
      ((guile-user))
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  As you can see, we don't do much other than creating a <TeXmacs> document.
  The load handler won't be complicated either. We only parse the query
  string with the help of <scm|query-ref> and then display one of three
  possible buffers.

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tmfs-load-handler (simple qry)

      \ \ (let ((type (query-ref qry "type"))

      \ \ \ \ \ \ \ \ (what (query-ref qry "what")))

      \ \ \ \ (tm-\<gtr\>stree

      \ \ \ \ \ \ (cond ((== type "very") (simple-load "Very simple" what))

      \ \ \ \ \ \ \ \ \ \ \ \ ((== type "totally") (simple-load "Totally
      simple" what))

      \ \ \ \ \ \ \ \ \ \ \ \ (else (simple-load "Error"

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (string-append "Query
      unknown: " what)))))))
    <|unfolded-io>
      #\<less\>procedure #f (qry)\<gtr\>
    </unfolded-io>
  </session>

  We can test this right away with:

  <\session|scheme|default>
    <\input>
      Scheme]\ 
    <|input>
      (load-buffer "tmfs://simple/type=very&what=example")
    </input>
  </session>

  Or embedded in a document using tags like <markup|hlink> and
  <markup|branch>: <hlink|click here to test
  it|tmfs://simple/type=very&what=example>.

  You can set read/write permissions implementing a <em|permission handler>,
  and the window's title using a <em|title handler>:

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (tmfs-permission-handler (simple name type)\ 

      \ \ (display* "Name= " name "\\nType= " type "\\n")

      \ \ #t)
    <|unfolded-io>
      #\<less\>procedure #f (name type)\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (tmfs-title-handler (simple qry doc) "Simple handler - Some title
      here")
    <|unfolded-io>
      #\<less\>procedure #f (qry doc)\<gtr\>
    </unfolded-io>
  </session>

  <\explain>
    <scm|(tmfs-load-handler (<scm-arg|name> <scm-arg|qry>)
    <scm-arg|body>)><explain-synopsis|define load handler for @name>
  <|explain>
    A <em|load handler> for <scm-arg|name> is invoked when <TeXmacs> receives
    a request to open a <abbr|URI> of type
    <tt|tmfs://<scm-arg|name>/<scm-arg|qry>>. The <scm-arg|body> of the
    handler is passed <tt|qry> as parameter (see <scm|query-ref>) and must
    return a complete <TeXmacs> buffer. Consider the following example:

    <\scm-code>
      (tmfs-load-handler (id qry)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document ,qry))))
    </scm-code>

    This will open <abbr|URI>s with the format
    <tt|tmfs://id/whatever_arguments>.

    Creation of the buffer contents may be simplified using the procedures
    defined in module <tt|<hlink|kernel.gui.gui-markup|tmfs://sapi/type=module&what=kernel.gui.gui-markup>>.
  </explain>

  <\explain>
    <scm|(tmfs-save-handler (<scm-arg|name> <scm-arg|qry> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define save handler for @name>
  </explain|A <em|save handler> is invoked when the user tries to save a
  buffer of type <tt|tmfs://<scm-arg|name>/...> See also
  <scm|tmfs-load-handler> and others.>

  <\explain>
    <scm|(tmfs-title-handler (<scm-arg|name> <scm-arg|qry> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define title handler @name>
  </explain|A <em|title handler> is invoked to build the title for a window
  displaying a buffer of type <tt|tmfs://<scm-arg|name>/...> It is expected
  to return a simple string in the right language for the user.>

  <\explain>
    <scm|(tmfs-permission-handler (<scm-arg|name> <scm-arg|qry>
    <scm-arg|kind>) <scm-arg|body>)><explain-synopsis|define master handler
    @name>
  </explain|A <em|permissions handler> decides whether the buffer
  corresponding to the query made to the handler may be loaded/saved, etc.
  <scm-arg|kind> may take one of the values <scm|"load">, (...)>

  <\explain>
    <scm|(tmfs-master-handler (<scm-arg|name> <scm-arg|qry> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define title handler @name>
  </explain|A <em|master handler> is...>

  <\explain>
    <scm|(query-ref <scm-arg|qry> <scm-arg|arg>)><explain-synopsis|return
    value of parameter @arg in query @qry>
  </explain|Given a <scm-arg|qry> string of type
  <tt|variable1=value1&variable2=value2>, <scm|query-ref> will return
  <tt|value1> for an <scm-arg|arg> value of <scm|value1>, etc.>

  <subsection|Installing the handler>

  In order to make your handler available from any menu item or document upon
  startup, you must add it to the initialization process, that is to
  <tt|init-texmacs.scm> or <tt|my-init-texmacs.scm>, using the macro
  <scm|lazy-tmfs-handler>. This will delay loading of your code either until
  it is required or <TeXmacs> is idle waiting for user input.

  <\remark>
    \ The keywords <tt|buffer>, <tt|view> and <tt|window> may not be used as
    names for handlers since they are used internally by <TeXmacs>.
  </remark>

  <\explain>
    <scm|(lazy-tmfs-handler <scm-arg|module>
    <scm-arg|handler>)><explain-synopsis|lazily install a tmfs handler>
  <|explain>
    Inform <TeXmacs> that <scm-arg|handler> is available in module
    <scm-arg|module>. <scm-arg|module> must be a list of symbols (like
    <scm|(kernel gui gui-markup)>) representing the <scheme> modle wher
    you'll have defined your handler using <scm|tmfs-handler> or with the
    convenience macros <scm|tmfs-load-handler>, <scm|tmfs-save-handler>,
    <scm|tmfs-permission-handler> and <scm|tmfs-title-handler>.
  </explain>

  <subsection|More advanced usage>

  The <TeXmacs> file system is actually a much more complicated beast, with
  versioning, network access and authentication built in among other things.
  This documentation should be completed with all those features.

  \;

  <tmdoc-copyright|2012|the <TeXmacs> team.>
</body>