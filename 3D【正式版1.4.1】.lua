--[[自定义部分]]--
accuracy=0
--光源方向
light_vec={0,0,1}
--light_color
light_c="&HFFFFFF&"
--默认底色
default_color="&HFFFFFF&"
--自然光
n_light=0.5

--自定义参数
mat={}
mat.accuracy=accuracy or 1
light_vec=light_vec or {0,0,1}
n_light=n_light or 0.5
--定义矩阵
mat.set=function(tbl)
	matrixs_metatable={
		__add=function(m,tbl)
			if _G.type(tbl) == "table" and _G.type(tbl[1]) == "number" and #tbl == 3 then
				for i=1,#m do
					m[i][1]=m[i][1]+tbl[1]
					m[i][2]=m[i][2]+tbl[2]
					m[i][3]=m[i][3]+tbl[3]
				end
			elseif _G.type(tbl) == "table" and _G.type(tbl[1]) == "table" then
				for i=1,#tbl do
					if #tbl[i] == 3 then
						m[#m+1] = {tbl[i][1],tbl[i][2],tbl[i][3],s=tbl[i].s or nil}
					end
				end
			else
				_G.error("wrong table [__add]")
			end
			return m
		end,
		__mul=function(m,n)
			if _G.type(n) == "number" then
				for i=1,#m do
					for j=1,3 do
						m[i][j]=m[i][j]*n
					end
				end
				return m
			elseif _G.type(n) == "table" then
				if #n == 3 then
					if _G.type(n[1]) == "table" and #n[1] == 3 then
						local res_matrix={}
						for i=1,#m do
							res_matrix[i]={}
							for j=1,3 do
								res_matrix[i][j]=m[i][1]*n[1][j]+m[i][2]*n[2][j]+m[i][3]*n[3][j]
							end
						end
						return mat.set(res_matrix)
					elseif _G.type(n[1]) == "number" then
						for i=1,#m do
							for j=1,3 do
								m[i][j]=m[i][j]*n[j]
							end
						end
						return m
					else
						_G.error("wrong table [__mul] 1")
					end
				else
					_G.error("wrong table [__mul] 2")
				end
			else
				_G.error("wrong table [__mul] 3")
			end
		end
	}
	return _G.setmetatable(tbl,matrixs_metatable)
end
--旋转
mat.rotate=function(axis,theta)
	theta=theta/180*math.pi
	if axis == "x" then
		r_matrix={{1,0,0},{0,math.cos(theta),-math.sin(theta)},{0,math.sin(theta),math.cos(theta)}}
	elseif axis == "y" then
		r_matrix={{math.cos(theta),0,-math.sin(theta)},{0,1,0},{math.sin(theta),0,math.cos(theta)}}
	elseif axis == "z" then
		r_matrix={{math.cos(theta),-math.sin(theta),0},{math.sin(theta),math.cos(theta),0},{0,0,1}}
	else
		_G.error("请选择正确的旋转轴：x/y/z")
	end
	return r_matrix
end
--面变换  vec1 -> {0,0,1} -> vec2
mat.s_rotate=function(vec1,vec2)
	if vec1[1] == 0 and vec1[3] == 0 then
		sin=0
		cos=1
	else
		sin=-vec1[1]/vec_len({vec1[1],vec1[3]})
		cos=vec1[3]/vec_len({vec1[1],vec1[3]})
	end
	m=mat.set({{cos,0,-sin},{0,1,0},{sin,0,cos}})
	vec_xz1={vec_len({vec1[1],vec1[3]}),vec1[2]}
	vec_xz2={vec_len({vec2[1],vec2[3]}),vec2[2]}
	cos=in_product(vec_xz1,vec_xz2)/vec_len(vec_xz1)/vec_len(vec_xz2)
	sin=math.sqrt(1-cos^2)
	if vec_xz1[1]*vec_xz2[2] < vec_xz1[2]*vec_xz2[1] then
		sin=-sin
	end
	m=m*{{1,0,0},{0,cos,-sin},{0,sin,cos}}
	if vec2[1] == 0 and vec2[3] == 0 then
		sin=0
		cos=1
	else
		sin=vec2[1]/vec_len({vec2[1],vec2[3]})
		cos=vec2[3]/vec_len({vec2[1],vec2[3]})
	end
	m=m*{{cos,0,-sin},{0,1,0},{sin,0,cos}}
	return m
end
--自定义旋转轴
mat.l_rotate=function(vec,theta)
	return mat.s_rotate(vec,{0,0,1})*mat.rotate("z",theta)*mat.s_rotate({0,0,1},vec)
end
--矩阵table深复制
mat.copy=function(m)
	local new_matrix={}
	for i=1,#m do
		new_matrix[i]={m[i][1],m[i][2],m[i][3]}
		new_matrix[i].s=m[i].s or false
	end
	return mat.set(new_matrix)
end
--仅供测试用
mat.test=function(tbl)
	local text=""
	for x=1,#tbl do
		for y=1,3 do
			text=text..tbl[x][y].."  "
		end
		text=text.."\n"
	end
	return text
end
--向量外积
out_product=function(v1,v2)
	if #v1 == #v2 then
		if #v1 == 2 and #v2 == 2 then
			return v1[1]*v2[2]-v1[2]*v2[1]
		elseif #v1 == 3 and #v2 == 3 then
			return {v1[2]*v2[3]-v1[3]*v2[2],v1[3]*v2[1]-v1[1]*v2[3],v1[1]*v2[2]-v1[2]*v2[1]}
		end
	else
		_G.error("无效的外积")
	end
end
--向量内积
in_product=function(v1,v2)
	if #v1 == #v2 then
		local i_p=0
		for i=1,#v1 do
			i_p=i_p+v2[i]*v1[i]
		end
		return i_p
	else
		_G.error("无效的内积")
	end
end
--定义向量
set_vec=function(p1,p2)
	if #p1 == #p2 then
		local v={}
		for i=1,#p1 do
			v[i]=p2[i]-p1[i]
		end
		return v
	else
		_G.error("无效的向量")
	end
end
--点集去重
points_dc=function(tbl)
	local xi=2
	local tbl_dc=function(tbl1,tbl2)
		if #tbl1 ~= #tbl2 then
			_G.error("元素数不一致")
		end
		for i=1,#tbl1 do
			if tbl1[i] ~= tbl2[i] then
				return false
			end
		end
		return true
	end
	while xi <= #tbl do
		local res=false
		for i=1,xi-1 do
			if tbl_dc(tbl[xi],tbl[i]) then
				res=true
			end
		end
		if res then
			_G.table.remove(tbl,xi)
		else
			xi=xi+1
		end
	end
end
to_shape=function(tbl)
 	local s="m ".._G.table.concat(tbl[1]," ",1,2).." "
 	for i=2,#tbl do
		s=s.."l ".._G.table.concat(tbl[i]," ",1,2).." "
	end
 	return s
end
--计算向量长度
vec_len=function(vec)
	local len=0
	for i=1,#vec do
		len=len+vec[i]^2
	end
	return math.sqrt(len)
end
--取整
set_point=function(point)
	local p={}
	mat.accuracy=mat.accuracy or 0
	local macc=10^mat.accuracy
	for i=1,#point do
		p[i]=math.floor(macc*point[i]+0.5)/macc
	end
	return p
end
--光影
light_color=function(c1,c2,pct)
	if pct > 1 then
		pct = 1
	elseif pct < 0 then
		pct = 0
	end
	if c2 == "" then
		c2="&H000000&"
		pct=1-pct
	end
	local b1,g1,r1=string.match(c1,"&H(%x%x)(%x%x)(%x%x)&")
	local b2,g2,r2=string.match(c2,"&H(%x%x)(%x%x)(%x%x)&")
	local r=_G.tonumber(r1,16)*pct+_G.tonumber(r2,16)*(1-pct)
	local g=_G.tonumber(g1,16)*pct+_G.tonumber(g2,16)*(1-pct)
	local b=_G.tonumber(b1,16)*pct+_G.tonumber(b2,16)*(1-pct)
	return string.format("&H%02X%02X%02X&",b,g,r)
end
--法向量
get_nvec=function(points,o)
	local vec12=set_vec(points[1],points[2])
	local vec13=set_vec(points[1],points[3])
	n_vec=out_product(vec12,vec13)
	if o then
		local f_vec=set_vec(o,points[1])
		local c_theta=in_product(n_vec,f_vec)/(vec_len(n_vec)*vec_len(f_vec))
		if c_theta < 0 then
			n_vec={-n_vec[1],-n_vec[2],-n_vec[3]}
		end
	end
	return n_vec
end

get_shapes=function(matrixs_tbl,surface,alpha)
	alpha=alpha or 0
	o={0,0,0}
	local opn=0
	for i=1,#matrixs_tbl do
		if not matrixs_tbl[i].s then
			o[1]=o[1]+matrixs_tbl[i][1]
			o[2]=o[2]+matrixs_tbl[i][2]
			o[3]=o[3]+matrixs_tbl[i][3]
			opn=opn+1
		end
	end
	o={o[1]/opn,o[1]/opn,o[1]/opn}
	local final_shapes={}
	for si=1,#surface do
		local points={}
		for pi = 1,#surface[si] do
			points[#points+1]=matrixs_tbl[surface[si][pi]]
		end
		points_dc(points)
		if #points >= 3 then
			n_vec=get_nvec(points,o)
			if n_vec[3] > 0 then
				l_theta=math.deg(math.acos(in_product(n_vec,light_vec)/(vec_len(n_vec)*vec_len(light_vec))))/90-n_light
				s_layer=1
			elseif n_vec[3] <= 0 and alpha ~= 0 then
				n_vec={-n_vec[1],-n_vec[2],-n_vec[3]}
				l_theta=(math.deg(math.acos(in_product(n_vec,light_vec)/(vec_len(n_vec)*vec_len(light_vec))))/90-n_light)/alpha
				s_layer=0
			end
			if n_vec[3] > 0 or alpha ~= 0 then
				local point_tbl={}
				for pi = 1,#points do
					points[pi]=set_point(points[pi])
				end
				points_dc(points)
				if #points >= 3 then
					final_shapes[#final_shapes+1]={text=to_shape(points),color=light_color(surface[si].color or default_color,light_c,l_theta),layer=s_layer,clip=""}
					if surface[si].shapes and _G.type(surface[si].shapes) == "table" then
						s_shape=final_shapes[#final_shapes].text
						for ssi=1,#surface[si].shapes do
							local spi=surface[si].shapes[ssi].si-1
							local f_shape=string.gsub(surface[si].shapes[ssi].text,"([%d%-]+) ([%d%-]+)",function() spi=spi+1 return string.format("%d %d",math.round(matrixs_tbl[spi][1],mat.accuracy),math.round(matrixs_tbl[spi][2],mat.accuracy)) end)
							final_shapes[#final_shapes+1] = {text=f_shape,
								color=light_color(surface[si].shapes[ssi].color or default_color,light_c,l_theta),
								clip=surface[si].shapes[ssi].intersected_or_not == 1 and s_shape or "",
								layer=s_layer+(s_layer*2-1)*(surface[si].shapes[ssi].layer or 0),s=surface[si].shapes[ssi].s}
						end
					end
				end
			end
		end
	end
	return final_shapes
end

move_3d=function(st,et,dt,matrix,surface,alpha,matrix_fun)
	if matrix_fun == nil then
		matrix_fun=function(m,s,ti,tn) return m end
	end
	intersected_or_not=intersected_or_not or 1
	if j == 1 then
		ti=1
		loop_i=0
		tn=math.ceil((et-st)/dt)
		layer=line.layer
		local m=mat.copy(matrix)
		shapes=get_shapes(matrix_fun(m,surface,ti,tn),surface,alpha)
		maxloop(#shapes+1)
	end
	if j == maxj then
		if tn-ti >= 2 then
			ti=ti+1
			loop_i=0
			local m=mat.copy(matrix)
			shapes=get_shapes(matrix_fun(m,surface,ti,tn),surface,alpha)
			loopctl(2,#shapes+2)
		elseif tn-ti == 1 then
			ti=ti+1
			loop_i=0
			local m=mat.copy(matrix)
			shapes=get_shapes(matrix_fun(m,surface,ti,tn),surface,alpha)
			loopctl(2,#shapes+1)
		end
	end
	loop_i=loop_i+1
	retime("abs",line.start_time+st+dt*(ti-1),line.start_time+st+dt*ti)
	m3d=shapes[loop_i]
	relayer(layer+shapes[loop_i].layer)
	return ""
end

set_surface_shape=function(matrix,surface,surface_shape)
	o={0,0,0}
	local opn=0
	for i=1,#matrix do
		if not matrix[i].s then
			o[1]=o[1]+matrix[i][1]
			o[2]=o[2]+matrix[i][2]
			o[3]=o[3]+matrix[i][3]
			opn=opn+1
		end
	end
	o={o[1]/opn,o[1]/opn,o[1]/opn}
	for i=1,#surface_shape do
		local points={matrix[surface[surface_shape[i][1]][1]],matrix[surface[surface_shape[i][1]][2]],matrix[surface[surface_shape[i][1]][3]]}
		surface[surface_shape[i][1]].shapes=surface[surface_shape[i][1]].shapes or {}
		n_vec=get_nvec(points,o)
		local s_points={}
		local f_r=math.abs(points[1][1]*n_vec[1]+points[1][2]*n_vec[2]+points[1][3]*n_vec[3])/vec_len(n_vec)
		for x,y in string.gmatch(surface_shape[i].text,"([%d%-]+) ([%d%-]+)") do
			s_points[#s_points+1]={_G.tonumber(x),_G.tonumber(y),f_r}
		end
		s_points=mat.set(s_points)*mat.s_rotate({0,0,1},n_vec)
		local s_shape_tbl={si=#matrix+1,text=surface_shape[i].text,color=surface_shape[i].color or default_color,layer=surface_shape[i].layer or 1,intersected_or_not=surface_shape[i][2] or 1,s=surface_shape[i].name or true}
		for si=1,#s_points do
			matrix[#matrix+1] = {s_points[si][1],s_points[si][2],s_points[si][3],s=surface_shape[i].name or true}
		end
		s_shape_tbl.ei=#matrix
		_G.table.insert(surface[surface_shape[i][1]].shapes,s_shape_tbl)
	end
end

set_cube=function(x,y,z,color_tbl)
	cube_points=mat.set({{0,0,0},{x,0,0},{0,y,0},{0,0,z},{x,y,0},{x,0,z},{0,y,z},{x,y,z}})+{-x/2,-y/2,-z/2}
	cube_surface={{2,5,8,6},{3,5,8,7},{4,6,8,7},{1,3,7,4},{1,2,6,4},{1,2,5,3}}
	if _G.type(color_tbl) == "table" and #color_tbl == 6 then
		for i=1,6 do
			cube_surface[i].color=color_tbl[i]
		end
	end
	return cube_points,cube_surface
end

set_diamond=function(l1,l2,d1,d2,n,c)
	c=c or default_color
	diamond_points={{0,d2,0}}
	diamond_surface={{color=c}}
	local p_c=function(num)
		if num > 2*n+1 then
			return num-2*n
		else
			return num
		end
	end
	for i=1,n do
		diamond_points[#diamond_points+1]={l1*math.cos(math.pi/n*2*i),-d1,l1*math.sin(math.pi/n*2*i)}
		diamond_points[#diamond_points+1]={l2*math.cos(math.pi/n*(2*i+1)),0,l2*math.sin(math.pi/n*(2*i+1))}
		diamond_surface[1][#diamond_surface[1]+1]=2*i
		diamond_surface[#diamond_surface+1]={2*i,2*i+1,p_c(2*i+2),color=c}
		diamond_surface[#diamond_surface+1]={p_c(2*i+3),2*i+1,p_c(2*i+2),color=c}
		diamond_surface[#diamond_surface+1]={p_c(2*i+3),2*i+1,1,color=c}
	end
	return diamond_points,diamond_surface
end

m_fun=function(m,s,ti,tn)
	for si=1,#s do
		if s[si].shapes then
			n_vec=get_nvec({m[s[si][1]],m[s[si][2]],m[s[si][3]]},{0,0,0})
			for ssi=1,#s[si].shapes do
				for pi=s[si].shapes[ssi].si,s[si].shapes[ssi].ei do
					local pi_m=mat.set({m[pi]})*mat.s_rotate(n_vec,{0,0,1})*mat.rotate("z",20*ti)*{(3+2*math.sin(ti/5)),(3+2*math.sin(ti/5)),1}*mat.s_rotate({0,0,1},n_vec)
					m[pi]=pi_m[1]
				end
			end
		end
	end
	return m*mat.rotate("y",10*ti)*mat.rotate("x",-30)*(0.7+0.3*math.sin(ti/5))
end

set_pyramid=function(a,h,n,mode)
	pyramid_points={}
	pyramid_surface={{}}
	local r=a/2/math.sin(math.pi/n)
	if mode then
		h=math.sqrt(a^2-r^2)
	end
	for i=1,n do
		pyramid_points[i]={r*math.cos(math.pi*2/n*i),0,r*math.sin(math.pi*2/n*i)}
		pyramid_surface[1][i]=i
		pyramid_surface[i+1]={n+1,i,i+1 > n and 1 or i+1}
	end
	pyramid_points[n+1]={0,-h,0}
	return mat.set(pyramid_points),pyramid_surface
end

set_prism=function(a,h,n)
	prism_points={}
	prism_surface={{},{}}
	local r=a/2/math.sin(math.pi/n)
	for i=1,n do
		prism_points[2*i-1]={r*math.cos(math.pi*2/n*i),h,r*math.sin(math.pi*2/n*i)}
		prism_points[2*i]={r*math.cos(math.pi*2/n*i),-h,r*math.sin(math.pi*2/n*i)}
		prism_surface[1][i]=2*i-1
		prism_surface[2][i]=2*i
		prism_surface[i+2]={2*i-1,2*i,2*i+2 > 2*n and 2 or 2*i+2,2*i+1 > 2*n and 1 or 2*i+1}
	end
	return mat.set(prism_points),prism_surface
end