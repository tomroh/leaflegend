testthat::test_that('symbol tests', {
  # test args
  testthat::expect_error(makeSymbol('notashape', width = 1, color = 'black'))
  testthat::expect_error(makeSymbol('rect', width = '1', color = 'black'))
  testthat::expect_error(makeSymbol('rect', width = 1, height = '1', color = 'black'))
  testthat::expect_error(makeSymbol('rect', width = 1, opacity = '1', color = 'black'))
  testthat::expect_error(makeSymbol('rect', width = 1, opacity = 1, fillOpacity = '1', color = 'black'))
  testthat::expect_error(makeSymbolIcons('notashape', width = 1, color = 'black'))
  testthat::expect_error(makeSymbolIcons('rect', width = '1', color = 'black'))
  testthat::expect_error(makeSymbolIcons('rect', width = 1, height = '1', color = 'black'))
  testthat::expect_error(makeSymbolIcons('rect', width = 1, opacity = '1', color = 'black'))
  testthat::expect_error(makeSymbolIcons('rect', width = 1, opacity = 1, fillOpacity = '1', color = 'black'))
  testthat::expect_error(makeSymbolIcons(NA, width = 1, height = 1, opacity = 1, color = 'black'))

  # test shapes
  testthat::expect_equal(URLdecode(makeSymbol('rect', width = 1, height = 2,
                                    color = 'black', fillColor = 'blue',
                                    opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <rect id=\"rect\" x=\"1\" y=\"1\" height=\"2\" width=\"1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></rect>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('circle', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <circle id=\"circle\" cx=\"2\" cy=\"2\" r=\"1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></circle>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('triangle', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"triangle\" points=\"1,3 2,3 1.5,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('plus', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"plus\" points=\"1.4,1 1.4,1.8 1,1.8 1,2.2 1.4,2.2 1.4,3 1.6,3 1.6,2.2 2,2.2 2,1.8 1.6,1.8 1.6,1 1.4,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('cross', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"cross\" points=\"1.14142135623731,1 1,1.28284271247462 1.35857864376269,2 1,2.71715728752538 1.14142135623731,3 1.5,2.28284271247462 1.85857864376269,3 2,2.71715728752538 1.64142135623731,2 2,1.28284271247462 1.85857864376269,1 1.5,1.71715728752538 1.14142135623731,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('diamond', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"diamond\" points=\"1.5,1 1,2 1.5,3 2,2 1.5,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('star', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"star\" points=\"1.4,1 1.4,1.5171572 1.1414214,1 1,1.2828428 1.2585786,1.8 1,1.8 1,2.2 1.2585786,2.2 1,2.7171572 1.1414214,3 1.4,2.4828428 1.4,3 1.6,3 1.6,2.4828428 1.8585786,3 2,2.7171572 1.7414214,2.2 2,2.2 2,1.8 1.7414214,1.8 2,1.2828428 1.8585786,1 1.6,1.5171572 1.6,1 1.4,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('stadium', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <rect id=\"stadium\" x=\"1\" y=\"1\" height=\"2\" width=\"1\" rx=\"25%\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></rect>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('line', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <line id=\"line\" x1=\"0\" x2=\"3\" y1=\"2\" y2=\"2\" stroke=\"black\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></line>\n</svg>')
  testthat::expect_equal(URLdecode(makeSymbol('polygon', width = 1, height = 2,
                                              color = 'black', fillColor = 'blue',
                                              opacity = .9, fillOpacity = .7)),
                         'data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"3\" height=\"4\">\n  <polygon id=\"polygon\" points=\"1.5,1 1.02447174185242,1.69098300562505 1.20610737385376,2.80901699437495 1.79389262614624,2.80901699437495 1.97552825814758,1.69098300562505 1.5,1\" stroke=\"black\" fill=\"blue\" stroke-opacity=\"0.9\" fill-opacity=\"0.7\"></polygon>\n</svg>')

})
