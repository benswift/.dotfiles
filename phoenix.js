// Ben Swift's phoenix WM config file

// mostly pulled from
// https://github.com/carlo/bash-it/blob/master/dotfiles/.phoenix.js

var mash = [ 'cmd', 'alt', 'ctrl' ],
  mashMove = [ 'alt', 'ctrl' ],
  mashMoveMore = [ 'shift', 'alt', 'ctrl' ],
  nudgePixels = 10,
  padding = 4,
  previousSizes = {};


// ### General key configurations
//
// My space key together with `Ctrl`+`Cmd`+`Alt` is toggling any window
// between full screen and its initial size and position.
api.bind( 'space', mash, function() {
  Window.focusedWindow().toggleFullscreen();
});

// My keypad keys together with `Ctrl`+`Cmd`+`Alt` are set up to push any
// window to either 25% or 50%:
//
// - top-left (NW)
// - top half (N)
// - top-right (NE)
// - left half (E)
// - …
api.bind( 'pad9', mash, function() {
  Window.focusedWindow().toNE();
});

api.bind( 'pad8', mash, function() {
  Window.focusedWindow().toN();
});

api.bind( 'pad7', mash, function() {
  Window.focusedWindow().toNW();
});

api.bind( 'pad6', mash, function() {
  Window.focusedWindow().toE();
});

api.bind( 'pad4', mash, function() {
  Window.focusedWindow().toW();
});

api.bind( 'pad3', mash, function() {
  Window.focusedWindow().toSE();
});

api.bind( 'pad2', mash, function() {
  Window.focusedWindow().toS();
});

api.bind( 'pad1', mash, function() {
  Window.focusedWindow().toSW();
});

api.bind( 'pad-', mash, function() {
  Window.focusedWindow().toGrid( 0.25, 0, 0.5, 1 );
});

// The cursor keys together with `Ctrl`+`Cmd`+`Alt` make any window occupy any
// half of the screen (N, E, S, W).
api.bind( 'up', mash, function() {
  Window.focusedWindow().toN();
});

api.bind( 'right', mash, function() {
  Window.focusedWindow().toE();
});

api.bind( 'down', mash, function() {
  Window.focusedWindow().toS();
});

api.bind( 'left', mash, function() {
  Window.focusedWindow().toW();
});

// The cursor keys together with `Ctrl`+`Alt` move a window.
api.bind( 'up', mashMove, function() {
  Window.focusedWindow().nudgeUp();
});

api.bind( 'right', mashMove, function() {
  Window.focusedWindow().nudgeRight();
});

api.bind( 'down', mashMove, function() {
  Window.focusedWindow().nudgeDown();
});

api.bind( 'left', mashMove, function() {
  Window.focusedWindow().nudgeLeft();
});


// The cursor keys together with `Shift`+`Ctrl`+`Alt` move a window just like
// the previous set but 5 times as fast.
api.bind( 'up', mashMoveMore, function() {
  Window.focusedWindow().nudgeUp( 5 );
});

api.bind( 'right', mashMoveMore, function() {
  Window.focusedWindow().nudgeRight( 5 );
});

api.bind( 'down', mashMoveMore, function() {
  Window.focusedWindow().nudgeDown( 5 );
});

api.bind( 'left', mashMoveMore, function() {
  Window.focusedWindow().nudgeLeft( 5 );
});


// #### Apps: Chrome + Sublime
//
// When working on frontend stuff I like my Sublime Text to cover the left
// (East) half of the screen and Chrome the right (West) half.
api.bind( '1', mash, function() {
  var chromeApp = App.findByTitle('Google Chrome'),
    chromeWindow = chromeApp && chromeApp.findWindowNotMatchingTitle('^Developer Tools -'),
    sublimeApp = App.findByTitle('Sublime Text'),
    sublimeWindow = sublimeApp && sublimeApp.firstWindow();

  api.alert( 'Chrome + Sublime', 0.25 );

  if ( chromeWindow ) {
    chromeWindow.toE();
  }

  if ( sublimeWindow ) {
    sublimeWindow.toW();
  }
});

// #### Apps: Chrome Devtools
//
// When checking HTML/JS in Chrome I want to have my browsing window to the
// East and my Chrome devtools window to the W, the latter not quite on full
// height.
api.bind( '2', mash, function() {
  var chromeApp = App.findByTitle('Google Chrome'),
    browseWindow = chromeApp && chromeApp.findWindowNotMatchingTitle('^Developer Tools -'),
    devToolsWindow = chromeApp && chromeApp.findWindowMatchingTitle('^Developer Tools -');

  api.alert( 'Chrome Dev Tools Layout', 0.25 );

  if ( browseWindow ) {
    browseWindow.toE();
  }

  if ( devToolsWindow ) {
    devToolsWindow.toGrid( 0, 0, 0.5, 0.75 );
  }
});



// ### Helper methods `Window`
//
// #### Window#toGrid()
//
// This method can be used to push a window to a certain position and size on
// the screen by using four floats instead of pixel sizes.  Examples:
//
//     // Window position: top-left; width: 25%, height: 50%
//     someWindow.toGrid( 0, 0, 0.25, 0.5 );
//
//     // Window position: 30% top, 20% left; width: 50%, height: 35%
//     someWindow.toGrid( 0.3, 0.2, 0.5, 0.35 );
//
// The window will be automatically focussed.  Returns the window instance.
Window.prototype.toGrid = function( x, y, width, height ) {
  var screen = this.screen().frameWithoutDockOrMenu(),
    newFrame = {
      x: Math.round( x * screen.width ) + padding + screen.x,
      y: Math.round( y * screen.height ) + padding + screen.y,
      width: Math.round( width * screen.width ) - ( 2 * padding ),
      height: Math.round( height * screen.height ) - ( 2 * padding )
    };

  // When setting the `height` to 1, the padding isn't applied at the bottom
  // end of the frame.  (I guess it's a bug.)  Setting the frame to a height
  // less than `1` first is a workaround to counter that behaviour.
  if ( height === 1 ) {
    this.setFrame(
      _({}).extend( newFrame, { height: screen.height - 50 })
    );
  }

  this.setFrame( newFrame );
  this.focusWindow();

  return this;
};


// #### Window#toFullScreen()
//
// Convenience method, doing exactly what it says.  Returns the window
// instance.
Window.prototype.toFullScreen = function() {
  return this.toGrid( 0, 0, 1, 1 );
};


// #### Window#toN()
//
// Convenience method, pushing the window to the top half of the screen.
// Returns the window instance.
Window.prototype.toN = function() {
  return this.toGrid( 0, 0, 1, 0.5 );
};


// #### Window#toNE()
//
// Convenience method, pushing the window to the top-right quarter of the
// screen.  Returns the window instance.
Window.prototype.toNE = function() {
  return this.toGrid( 0.5, 0, 0.5, 0.5 );
};


// #### Window#toE()
//
// Convenience method, pushing the window to the right half of the screen.
// Returns the window instance.
Window.prototype.toE = function() {
  return this.toGrid( 0.5, 0, 0.5, 1 );
};


// #### Window#toSE()
//
// Convenience method, pushing the window to the bottom-right quarter of the
// screen.  Returns the window instance.
Window.prototype.toSE = function() {
  return this.toGrid( 0.5, 0.5, 0.5, 0.5 );
};


// #### Window#toS()
//
// Convenience method, pushing the window to the bottom half of the screen.
// Returns the window instance.
Window.prototype.toS = function() {
  return this.toGrid( 0, 0.5, 1, 0.5 );
};


// #### Window#toSW()
//
// Convenience method, pushing the window to the bottom-left quarter of the
// screen.  Returns the window instance.
Window.prototype.toSW = function() {
  return this.toGrid( 0, 0.5, 0.5, 0.5 );
};


// #### Window#toW()
//
// Convenience method, pushing the window to the left half of the screen.
// Returns the window instance.
Window.prototype.toW = function() {
  return this.toGrid( 0, 0, 0.5, 1 );
};


// #### Window#toNW()
//
// Convenience method, pushing the window to the top-left quarter of the
// screen.  Returns the window instance.
Window.prototype.toNW = function() {
  return this.toGrid( 0, 0, 0.5, 0.5 );
};


// #### Window#toggleFullscreen()
//
// Stores the window position and size, then makes the window full screen.
// Should the window be full screen already, its original position and size
// is restored.  Returns the window instance.
Window.prototype.toggleFullscreen = function() {
  if ( previousSizes[ this ] ) {
    this.setFrame( previousSizes[ this ] );
    delete previousSizes[ this ];
  }
  else {
    previousSizes[ this ] = this.frame();
    this.toFullScreen();
  }

  return this;
};


// #### Window#nudgeLeft()
//
// Move the currently focussed window left by [`nudgePixel`] pixels.
Window.prototype.nudgeLeft = function( factor ) {
  var win = Window.focusedWindow(),
    frame = win.frame(),
    pixels = nudgePixels * ( factor || 1 );

  frame.x -= ( frame.x >= pixels ) ? pixels : 0;
  win.setFrame( frame );
};



// #### Window#nudgeRight()
//
// Move the currently focussed window right by [`nudgePixel`] pixels.
Window.prototype.nudgeRight = function( factor ) {
  var win = Window.focusedWindow(),
    frame = win.frame(),
    maxLeft = win.screen().frameIncludingDockAndMenu().width - frame.width,
    pixels = nudgePixels * ( factor || 1 );

  frame.x += ( frame.x < maxLeft - pixels ) ? pixels : 0;
  win.setFrame( frame );
};


// #### Window#nudgeUp()
//
// Move the currently focussed window left by [`nudgePixel`] pixels.
Window.prototype.nudgeUp = function( factor ) {
  var win = Window.focusedWindow(),
    frame = win.frame(),
    pixels = nudgePixels * ( factor || 1 );

  frame.y -= ( frame.y >= pixels ) ? pixels : 0;
  win.setFrame( frame );
};



// #### Window#nudgeDown()
//
// Move the currently focussed window right by [`nudgePixel`] pixels.
Window.prototype.nudgeDown = function( factor ) {
  var win = Window.focusedWindow(),
    frame = win.frame(),
    maxLeft = win.screen().frameIncludingDockAndMenu().height - frame.height,
    pixels = nudgePixels * ( factor || 1 );

  frame.y += ( frame.y < maxLeft - pixels ) ? pixels : 0;
  win.setFrame( frame );
};


// ### Helper methods `App`
//
// #### App.findByTitle()
//
// Finds the window with a certain title.  Expects a string, returns a window
// instance or `undefined`.  If there are several windows with the same title,
// the first found instance is returned.
App.findByTitle = function( title ) {
  return _( this.runningApps() ).find( function( app ) {
    if ( app.title() === title ) {
      app.show();
      return true;
    }
  });
};


// #### App#findWindowMatchingTitle()
//
// Finds the window whose title matches a regex pattern.  Expects a string
// (the pattern), returns a window instance or `undefined`.  If there are
// several matching windows, the first found instance is returned.
App.prototype.findWindowMatchingTitle = function( title ) {
  var regexp = new RegExp( title );

  return _( this.visibleWindows() ).find( function( win ) {
    return regexp.test( win.title() );
  });
};


// #### App#findWindowNotMatchingTitle()
//
// Finds the window whose title doesn't match a regex pattern.  Expects a
// string (the pattern), returns a window instance or `undefined`.  If there
// are several matching windows, the first found instance is returned.
App.prototype.findWindowNotMatchingTitle = function( title ) {
  var regexp = new RegExp( title );

  return _( this.visibleWindows() ).find( function( win ) {
    return !regexp.test( win.title() );
  });
};


// #### App#firstWindow()
//
// Returns the first visible window of the app or `undefined`.
App.prototype.firstWindow = function() {
  return this.visibleWindows()[ 0 ];
};

