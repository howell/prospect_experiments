
var FRAME_PERIOD = 16;
var GRAVITY = 1;
var TERMINAL_VELOCITY = 10;
var PLAYER_WIDTH = 10;
var PLAYER_HEIGHT = 20;
var DX = 5;
var JUMP_V = -13;

function Point(x, y) {
  this.x = x;
  this.y = y;
}

function Rect(x, y, w, h) {
  this.x = x;
  this.y = y;
  this.w = w;
  this.h = h;
}

Rect.prototype.topLeft = function () {
  return new Point(this.x, this.y);
};

Rect.prototype.bottomRight = function() {
  return new Point(this.x + this.w, this.y + this.h);
};

Rect.prototype.overlaps = function (rect) {
  var tl1 = this.topLeft();
  var tl2 = rect.topLeft();
  var br1 = this.bottomRight();
  var br2 = rect.bottomRight();
  return (tl1.x < br2.x) && (br1.x > tl2.x) && (tl1.y < br2.y) && (br1.y > tl2.y);
};

// distance between the top left points of two Rect's
Rect.prototype.distanceTo = function(rect) {
  return Math.sqrt(Math.pow(this.x - rect.x, 2) + Math.pow(this.y - rect.y, 2));
};

// velocity and acceleration
function Motion(v, a) {
  this.v = v;
  this.a = a;
}

// int
function MoveX(id, dx) {
  this. id = id;
  this.dx = dx;
}

// int
function MoveY(id, dy, clock) {
  this. id = id;
  this.dy = dy;
  this.clock = clock;
}

function drawRectangle(ctx, rect, color) {
  ctx.fillStyle = color;
  ctx.fillRect(rect.x, rect.y, rect.w, rect.h);
}

// rect
function Player(x0, y0) {
  this.x = x0;
  this.y = y0;
  this.w = PLAYER_WIDTH;
  this.h = PLAYER_HEIGHT;
  this.dy = 0;
  this.wantToJump = false;
  this.left = 0;
  this.right = 0;
}

Player.prototype = Object.create(Rect.prototype);
Player.prototype.constructor = Player;

Player.prototype.draw = function(context, scrollX, scrollY) {
  drawRectangle(context, new Rect(this.x - scrollX, this.y - scrollY, this.w, this.h), "blue");
};

Player.prototype.keyDown = function(keyCode) {
  switch (keyCode) {
  case 37: // left arrow
    this.left = 1;
    break;
  case 39: // right arrow
    this.right = 1;
    break;
  case 32: // space
    this.wantToJump = true;
    break;
  }
};

Player.prototype.keyUp = function(keyCode) {
  switch (keyCode) {
  case 37: // left arrow
    this.left = 0;
    break;
  case 39: // right arrow
    this.right = 0;
    break;
  case 32: // space
    break;
  }

};

Player.prototype.tick = function(moveX, moveY, canJump) {
  if (this.wantToJump && canJump(this)) {
    this.dy = JUMP_V;
  }
  this.wantToJump = false;
  var dx = this.right * DX - this.left * DX;
  moveX(this, dx);
  if (moveY(this, this.dy)) {
    // vertical collisions set velocity back to 0
    this.dy = 0;
  }
  this.dy += GRAVITY;
  if (this.dy > TERMINAL_VELOCITY) {
    this.dy = TERMINAL_VELOCITY;
  }
}


// any * rect
function Enemy(x,y,w,h) {
  this.x = x;
  this.y = y;
  this.w = w;
  this.h = h;
}

Enemy.prototype = Object.create(Rect.prototype);
Enemy.prototype.constructor = Enemy;
Enemy.prototype.draw = function(context, scrollX, scrollY) {
    drawRectangle(context, new Rect(this.x - scrollX, this.y - scrollY, this.w, this.h), "red");
};
Enemy.prototype.tick = function(moveX, moveY) {
};

function HorizontalEnemy(x,y,w,h,xDist,dx) {
  this.x0 = x;
  this.x = x;
  this.y = y;
  this.w = w;
  this.h = h;
  this.xDist = xDist;
  this.dx = dx;
}

HorizontalEnemy.prototype = Object.create(Enemy.prototype);
HorizontalEnemy.prototype.constructor = HorizontalEnemy;
HorizontalEnemy.prototype.tick = function(moveX, moveY) {
  if (this.x > this.x0 + this.xDist || this.x < this.x0) {
    this.dx = - this.dx;
  }
  moveX(this, this.dx);
};

function VerticalEnemy(x,y,w,h,yDist,dy) {
  this.y0 = y;
  this.x = x;
  this.y = y;
  this.w = w;
  this.h = h;
  this.yDist = yDist;
  this.dy = dy;
}

VerticalEnemy.prototype = Object.create(Enemy.prototype);
VerticalEnemy.prototype.constructor = VerticalEnemy;
VerticalEnemy.prototype.tick = function(moveX, moveY) {
  if (this.y > this.y0 + this.yDist || this.y < this.y0) {
    this.dy = - this.dy;
  }
  moveY(this, this.dy);
};

// rect
function Goal(x0,y0,w,h) {
  this.x = x0;
  this.y = y0;
  this.w = w;
  this.h = h;
}

Goal.prototype = Object.create(Rect.prototype);
Goal.prototype.constructor = Goal;
Goal.prototype.draw = function(context, scrollX, scrollY) {
    drawRectangle(context, new Rect(this.x - scrollX, this.y - scrollY, this.w, this.h), "yellow");
};

// rect
function Static(x0,y0,w,h) {
  this.x = x0;
  this.y = y0;
  this.w = w;
  this.h = h;
}

Static.prototype = Object.create(Rect.prototype);
Static.prototype.constructor = Static;
Static.prototype.draw = function(context, scrollX, scrollY) {
    drawRectangle(context, new Rect(this.x - scrollX, this.y - scrollY, this.w, this.h), "black");
};

function Game(levels) {
  this.levelSpecs = levels;
  this.levelNumber = 0;
  this.currentLevel = null;

  this.start = function(canvas) {
    this.canvas = canvas;
    var self = this;
    this.timer = setInterval(() => this.tick(), FRAME_PERIOD);
    // might need to unregister these...
    $(document).keydown((e) => this.onKeyDown(e));
    $(document).keyup((e) => this.onKeyUp(e));
    this.currentLevel = this.levelSpecs[this.levelNumber]();
    this.currentLevel.start(this.canvas, () => this.onPlayerDeath(), () => this.onLevelCompleted());
  };

  this.tick = function() {
    if (this.currentLevel) {
      this.currentLevel.tick();
    }
  };

  this.onKeyDown = function(event) {
    var keyCode = event.which;
    if (this.currentLevel) {
      this.currentLevel.keyDown(keyCode);
    }
  };

  this.onKeyUp = function(event) {
    var keyCode = event.which;
    if (this.currentLevel) {
      this.currentLevel.keyUp(keyCode);
    }
  };

  this.onPlayerDeath = function () {
    this.currentLevel = this.levelSpecs[this.levelNumber]();
    this.currentLevel.start(this.canvas, () => this.onPlayerDeath(), () => this.onLevelCompleted());
  };

  this.onLevelCompleted = function() {
    this.levelNumber++;
    if (this.levelNumber < this.levelSpecs.length) {
      // boot next level
      this.currentLevel = this.levelSpecs[this.levelNumber]();
      this.currentLevel.start(this.canvas, () => this.onPlayerDeath(), () => this.onLevelCompleted());
    } else {
      // game complete!
      this.currentLevel = null;
      this.drawVictory();
    }
  };

  this.drawVictory = function () {
    var ctx = this.canvas.getContext("2d");
    ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    ctx.font = "60px Arial";
    ctx.fillStyle = "green";
    ctx.fillText("Victory!",this.canvas.width/3,this.canvas.height/2);
  };
}

// determine an offset for side-scrolling in a dimension
// player: the position of the player in that dimension
// canvas: the size of the canvas in that dimension
// level: the size of the level in that dimension
function scrollOffset(player, canvas, level) {
  // don't scroll when the level is smaller than the canvas
  if (level <= canvas) {
    return 0;
  }
  var halfC = canvas / 2;
  if (player - halfC < 0) {
    // don't scroll when the player is close to the beginning of the level
    return 0;
  } else if (player + halfC > level) {
    // similarly, don't scroll when near the end
    return level - canvas;
  } else {
    // otherwise put the player at the center of the screen
    return player - halfC;
  }
}

// rect * (listof rect) * rect * (-> (listof spawn)) * posn
function Level(player0, env0, goal, enemies, xMax, yMax) {
  this.player = player0;
  this.env = env0;
  this.goal = goal;
  this.enemies = enemies;
  this.xMax = xMax;
  this.yMax = yMax;

  this.start = function(canvas, onPlayerDeath, onLevelComplete) {
    this.canvas = canvas;
    this.onPlayerDeath = onPlayerDeath;
    this.onLevelComplete = onLevelComplete;
    this.draw();
  };

  this.draw = function() {
    var context = this.canvas.getContext('2d');
    context.clearRect(0, 0, this.canvas.width, this.canvas.height);
    var scrollX = scrollOffset(this.player.x, this.canvas.width, this.xMax);
    var scrollY = scrollOffset(this.player.y, this.canvas.height, this.yMax);
    for (var i = 0; i < this.env.length; i++) {
      this.env[i].draw(context, scrollX, scrollY);
    }
    for (var i = 0; i < this.enemies.length; i++) {
      this.enemies[i].draw(context, scrollX, scrollY);
    }
    this.goal.draw(context, scrollX, scrollY);
    this.player.draw(context, scrollX, scrollY);
  };

  this.tick = function() {
    var self = this;
    this.enemies.forEach((enemy) =>
                         enemy.tick((e,dx) => this.moveX(e,dx), (e,dy) => this.moveY(e,dy)));
    this.player.tick((p,dx) => this.moveX(p,dx), (p,dy) => this.moveY(p,dy),
                     (p) => this.playerCanJump(p));
    if (!this.player.overlaps(new Rect(0,0,this.xMax,this.yMax))) {
      // death by falling off level
      this.onPlayerDeath();
      return;
    }
    if (this.enemies.some((e) => this.player.overlaps(e))) {
      this.onPlayerDeath();
      return;
    }
    if (this.player.overlaps(this.goal)) {
      this.onLevelComplete();
      return;
    }
    this.draw();
  };

  this.keyDown = function(keyCode) {
    this.player.keyDown(keyCode);
  };

  this.keyUp = function(keyCode) {
    this.player.keyUp(keyCode);
  };

  this.playerCanJump = function (p) {
    var justBelow = new Rect(p.x, p.y + 1, p.w, p.h);
    return this.env.some((r) => justBelow.overlaps(r));
  };

  // return if a collision with the environment occurs
  // mutates first argument with new location
  this.moveX = function(r, dx) {
    var dest = new Rect(r.x + dx, r.y, r.w, r.h);
    var motion = dx < 0 ?
          new Rect(dest.x, dest.y, Math.abs(dx) + r.w, r.h) :
          new Rect(r.x, r.y, dx + r.w, r.h);
    var collisions = this.env.filter((env) => motion.overlaps(env));
    collisions.sort((a,b) => r.distanceTo(a) - r.distanceTo(b));
    if (collisions.length > 0) {
      var closest = collisions[0];
      var newX = r.x < closest.x ? closest.x - r.w : closest.x + closest.w;
      r.x = newX;
      return true;
    }
    r.x = dest.x;
    return false;
  };

  // return if a collision with the environment occurs
  // mutates first argument with new location
  this.moveY = function(r, dy) {
    var dest = new Rect(r.x, r.y + dy, r.w, r.h);
    var motion = dy < 0 ?
          new Rect(dest.x, dest.y, r.w, Math.abs(dy) + r.h) :
          new Rect(r.x, r.y, r.w, dy + r.h);
    var collisions = this.env.filter((env) => motion.overlaps(env));
    collisions.sort((a,b) => r.distanceTo(a) - r.distanceTo(b));
    if (collisions.length > 0) {
      var closest = collisions[0];
      var newY = r.y < closest.y ? closest.y - r.h : closest.y + closest.h;
      r.y = newY;
      return true;
    }
    r.y = dest.y;
    // ugh this is hacky. check to see if the player killed any enemies
    if (r instanceof Player && dy > 0) {
      this.enemies = this.enemies.filter((e) => ! r.overlaps(e));
    }
    return false;
  };

}

var level1 = () => new Level(new Player(0,0),
                             [new Static(0,200,150,10),
                              new Static(400,200,1000,10),
                              new Static(200,178,50,10),
                              new Static(300,150,50,10)],
                             new Goal(900,150,20,20),
                             [new HorizontalEnemy(0,180,20,20,130,2),
                              new HorizontalEnemy(200,158,20,20,30,1),
                              new HorizontalEnemy(300,130,20,20,30,1),
                              new HorizontalEnemy(400,180,20,20,180,3)],
                             1000, 400);

var level2 = () => new Level(new Player(0,0),
                             [new Static(0,200,600,10)],
                             new Goal(500,150,20,20),
                             [new HorizontalEnemy(0,180,20,20,580,4),
                              new HorizontalEnemy(0,140,20,20,580,8),
                              new VerticalEnemy(50,125,20,20,54,3),
                              new VerticalEnemy(100,125,20,20,54,3),
                              new VerticalEnemy(150,125,20,20,54,3),
                              new VerticalEnemy(200,125,20,20,54,3),
                              new VerticalEnemy(250,125,20,20,54,3),
                              new VerticalEnemy(300,125,20,20,54,3),
                              new VerticalEnemy(350,125,20,20,54,3)],
                             600,400);

function ascendingStairs(x0, y0, hdist, vdist, w, h, n) {
  var steps = [];
  for (var i = 0; i  < n; i++) {
    var dx = hdist * i;
    var dy = vdist * i;
    steps.push(new Static(x0 + dx, y0 + dy, w, h));
  }
  return steps;
}

function birdies() {
  var birds = [];
  for (var i = 0; i < 5; i++) {
    birds.push(new VerticalEnemy(160 + 200 * i, 650 - i * 80, 20, 20, 120, 4));
  }
  return birds;
}

var level3 = () => new Level(new Player(0,750),
                             ascendingStairs(50 + 50, 800 - 40, 100, -40, 50, 10, 10).concat([new Static(0,800,50,200)]),
                             new Goal(1100,950,20,20),
                             birdies(),
                             2000,1000);



var levels = [level1, level2,level3];

function runGame() {
  var canvas = document.getElementById('game-canvas');
  canvas.width = 600;
  canvas.height = 400;
  var ctx = canvas.getContext('2d');
  var game = new Game(levels);
  game.start(canvas);
}

function runLevel(lvl, canvas) {

}

$(document).ready(runGame);
