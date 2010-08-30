%
% matlab implementation of forest fire CA
%
% matt@galois.com
%

% probabilities
p = 0.96;  % grow
f = 0.999; % lightning

% size
sz = [150 150];

% constants with names
EMPTY   = 0;
TREE    = 1;
BURNING = 2;

% world starts empty
world = zeros(sz(1),sz(2));

% iterate for 10000 time steps
for i=1:10000
    % empties may come to life
    newgrowth = double(world==EMPTY).*double(rand(sz(1),sz(2))>p);

    tileworld = [world(sz(1),sz(2))  world(end,:)  world(sz(1),1); 
                 world(:,1)          world         world(:,end); 
                 world(1,sz(2))      world(1,:)    world(1,1)];

    % count neighbors.  tileworld gives us periodic boundaries for the
    % 2d convolution
    nconv = conv2(double(tileworld==BURNING),[1 1 1; 1 0 1; 1 1 1],'same');
    neighbors = nconv(2:end-1,2:end-1);

    % trees with burning neighbors will burn
    toburn = double(world==TREE).*double(neighbors > 0);
    
    % some trees get hit by lightning
    lightning = double(world==TREE).*double(rand(sz(1),sz(2))>f);
    
    % trees stay trees, and those that are burning get added on to
    % get value 2.
    trees = double(world==TREE)+double(toburn + lightning > 0);

    % the world is composed of new growth + trees (burning and not)
    world = (newgrowth + trees);
    
    % view the world
    imagesc(world);
    
    % keep colors ranging from 0 to 2
    caxis([0 2]);

    % plot
    drawnow;
end
