;;; coding: utf-8

(lazy-menu (satellite-menu) satellite-menu)

(define (satellite-initialize)
  (import-from (satellite))
  (tm-menu (tools-menu)
    (former)
    (-> "Satellite" (link satellite-menu))))

(plugin-configure plug-sat
  (:require #t)
  (:initialize (satellite-initialize)))
